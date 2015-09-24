/* Netfilter module that provides a variety of network functions */
#include "dsfield.h"
#include "flow_table.h"
#include "ip.h"
#include "helper.h"
#include "kulfi_mod.h"
#include "routing_table.h"
#include "vlan.h"
#include "proc/routes.h"
#include "proc/stats.h"

#define INTF_NAME "em2"

static struct nf_hook_ops nfho_post;

static struct proc_dir_entry * routes_proc_entry;
static struct proc_dir_entry * stats_proc_entry;

char proc_routes_data[MAX_ROUTES_SIZE];

flow_table_t * flow_table = NULL;

routing_table_t * routing_table = NULL;



/* Handle linearized sk_buff post_routing */
static unsigned int post_routing_process(const struct nf_hook_ops *ops,
        struct sk_buff *skb,
        const struct net_device *in,
        const struct net_device *out)
{
    char* ip_pkt;
    char* nip_pkt;
    int ip_pkt_len, nip_pkt_len;
    int eth_vlan_hdr_len, full_pkt_len, ip_hdr_len;
    int err;
    struct sk_buff *nskb;
    struct ethhdr *eth_hdr;
    struct flow_keys flow_key;
    struct iphdr *ip_hdr;
    struct iphdr *nip_hdr;
    struct stack stk;
    void *saddr;
    void *daddr;
    __be16 proto;
    u32 hash;
    __wsum nskb_csum = 0;
    unsigned char dst[] = {0xFF,0xFF,0xFF,0xFF,0xFF,0xFF};

    /*
    if(skb_is_nonlinear(skb)){
        pr_debug("Non-linear skb.. linearizing...\n");
        if(skb_linearize(skb)){
            pr_debug("Failed to serialize!\n");
        }
    }
    */

    if (skb_is_nonlinear(skb)){
        pr_debug("Proces_pkt: Still non-linear skb.\n");
        return NF_ACCEPT;
    }

    proto = ntohs(skb->protocol);
    pr_debug("Proto: %04x\n", proto);
    switch (proto) {
        case ETH_P_IP:
            eth_hdr = (struct ethhdr *) skb_mac_header(skb); // need to set
            ip_hdr = (struct iphdr *) skb_network_header(skb);
            ip_hdr_len = ip_hdrlen(skb);
            ip_pkt = (char *) ip_hdr;
            ip_pkt_len = ntohs(ip_hdr->tot_len);

            pr_debug("IP pkt_len = %d  IP hdr len = %d \n", ip_pkt_len,
                    ip_hdr_len);

            if(skb_flow_dissect(skb, &flow_key)){
                print_ip(flow_key.src);
                print_ip(flow_key.dst);
                pr_debug("Ports %d %d\n", ntohs(flow_key.port16[0]),
                        ntohs(flow_key.port16[1]));
                hash = flow_keys_hash(flow_key);
                pr_debug("Hash: %x\n", hash);
            }
            else{
                pr_debug("Failed to dissect flow\n");
                return NF_ACCEPT;
            }

            //stats_entry_inc(be32_to_cpu(flow_key.dst), ip_pkt_len);
            pr_debug("Proto: IP pkt\nGetting stk from flow_table for %u\n",
                    be32_to_cpu(flow_key.dst));
	    stk = flow_table_get(flow_table, flow_key, routing_table, be32_to_cpu(flow_key.dst));

	    if(stk.num_tags == -1) { // no_stack
                pr_debug("flow_table miss! consulting rt_table\n");
                stk = get_random_stack_for_dst(be32_to_cpu(flow_key.dst), routing_table);
                flow_table_set(flow_table, flow_key, stk);
            }

            if(stk.num_tags < 0) {
                stk.num_tags=0;
            }

            eth_vlan_hdr_len = ETH_HLEN + stk.num_tags * sizeof(vlan_label);
            full_pkt_len = eth_vlan_hdr_len + ip_pkt_len;
            pr_debug("Full length: %d", full_pkt_len);

            // Allocate new skb
            nskb = alloc_skb(full_pkt_len, GFP_ATOMIC);
            if (nskb == NULL) {
                return NF_ACCEPT;
            }
            if (skb->sk != NULL) {
                skb_set_owner_w(nskb, skb->sk);
            }
            else {
                kfree_skb(nskb);
                return NF_ACCEPT;
            }
            pr_debug("mod_vlan: nskb - Reserving header\n");

            // Reserve space for eth and vlan headers
            skb_reserve(nskb, eth_vlan_hdr_len);

            // Copy IP packet
            pr_debug("mod_vlan: copying IP pkt.\n");
            if (!(nip_pkt = skb_put(nskb, ip_pkt_len))) {
                pr_debug("skb_put failed!\n");
                kfree_skb(nskb);
                return NF_ACCEPT;
            }
            skb_reset_network_header(nskb);
            memcpy(nip_pkt, ip_pkt, ip_pkt_len);
            nip_hdr = (struct iphdr *) nip_pkt;
            nip_pkt_len = ntohs(nip_hdr->tot_len);

            nskb_csum = fix_csum(nip_hdr);

            // Set VLAN stack
            if (set_vlan_stack(nskb, &stk)) {
                proto = ETH_P_8021Q;
            }

            // Get outgoing interface
            nskb->dev = dev_get_by_name(&init_net, out->name);
            if (!nskb->dev) {
                pr_debug("mod_vlan dev_get_by_name (%s) FAILED.", out->name);
                kfree_skb(nskb);
                return NF_ACCEPT;
            }
/*
            // Reduce MTU, if needed
            if (nskb->dev->mtu > 1500 - (4 * stk.num_tags)) {
                pr_debug("Setting MTU: (%s) %u", out->name,
                        1500 - (4 * stk.num_tags));
                nskb->dev->mtu = 1500 - (4 * stk.num_tags);
            }
            pr_debug("mod_vlan dev_get_by_name success, nskb->dev->name='%s'",
                    nskb->dev->name);
*/
            saddr = nskb->dev->dev_addr;
            daddr = dst;

            // ARP Lookup
            if (get_dst_haddr(daddr, flow_key.dst, nskb->dev) != 0){
                pr_debug("ARP lookup - FAILED!\n");
                kfree_skb(nskb);
                return NF_ACCEPT;
            }

            // Set DL header
            print_mac(saddr);
            print_mac(daddr);

            pr_debug("calling dev_hard_header\n");
            if (!dev_hard_header(nskb, nskb->dev, proto,
                        daddr, saddr, nskb->dev->addr_len)) {
                pr_debug("mod_vlan dev_hard_header FAILED.\n");
                kfree_skb(nskb);
                return NF_ACCEPT;
            }

            skb_reset_mac_header(nskb);

            // Set skb checksum
            nskb->csum = nskb_csum;

            // Send out packet - dev_queue_xmit will consume nskb
            pr_debug("mod_vlan: sending nskb....\n");
            if ((err = dev_queue_xmit(nskb)) != NET_XMIT_SUCCESS) {
                pr_debug("mod_vlan dev_queue_xmit failed. %d\n", err);
                return NF_ACCEPT;
            }

            // Consume original skb
            consume_skb(skb);
            pr_debug("------ success - returning ------\n");
            return NF_STOLEN;
            break;
        default:
            pr_debug("Proto: Non-IP pkt\n");
            break;
    }
    // default: if we didn't send a new skb, then accept the original
    return NF_ACCEPT;
}

/* Packet handler */
static unsigned int process_pkt_post_routing(const struct nf_hook_ops *ops,
        struct sk_buff *skb,
        const struct net_device *in,
        const struct net_device *out,
        int (*okfn)(struct sk_buff *))
{
    int err = NF_STOLEN;

    if(out == NULL || out->name == NULL || strcmp(out->name, INTF_NAME) != 0){
        return NF_ACCEPT;
    }

    if(skb_is_nonlinear(skb)){
        pr_debug("Non-linear skb.\n");
        return NF_ACCEPT;
    }
    else{
        err = post_routing_process(ops, skb, in, out);
    }

    return err;
}

/* Initialization routine */
int init_module()
{
    nfho_post.hook     = process_pkt_post_routing;      /* Handler function */
    nfho_post.hooknum  = NF_INET_POST_ROUTING;
    nfho_post.pf       = AF_INET;
    nfho_post.priority = NF_IP_PRI_LAST;

    pr_debug("mod_vlan: Loading\n");
    // Create a flow_table [flow_match, vlan_stack]
    flow_table = flow_table_create(DEFAULT_FLOW_TABLE_SIZE);

    // Create a routing table [dst_ip, list_of_stacks]
    routing_table = routing_table_create(4096);

    // Create a proc file to read routing table from kulfi
    create_new_routes_proc_entry(routes_proc_entry);
    create_new_stats_proc_entry(stats_proc_entry);

    nf_register_hook(&nfho_post);

    return 0;
}

/* Cleanup routine */
void cleanup_module()
{
    nf_unregister_hook(&nfho_post);

    // delete flow_table (free copied stacks here)
    flow_table_delete(flow_table);
    kfree(flow_table);

    // delete routing table (free stacks here)
    routing_table_delete(routing_table);
    kfree(routing_table);

    // delete the /proc file
    delete_routes_proc_entry();
    delete_stats_proc_entry();

    pr_debug("mod_vlan: Exiting\n");
}

MODULE_AUTHOR("Praveen Kumar");
MODULE_LICENSE("GPL");
