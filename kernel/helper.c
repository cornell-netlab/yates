#include "helper.h"

void calc_transport_len(const struct sk_buff *skb,
        struct iphdr *ip_hdr, int *h_len, int *payload_length)
{
    struct tcphdr *tcph;
    struct udphdr *udph;
    pr_debug("In calc_transport_len\n");
    switch (ip_hdr->protocol) {
        case IPPROTO_TCP:
            tcph = (struct tcphdr *) ((u32 *) ip_hdr + ip_hdr->ihl);
            *h_len = tcph->doff * 4;
            *payload_length = ntohs(ip_hdr->tot_len) - ip_hdrlen(skb) - (*h_len);
            break;
        case IPPROTO_UDP:
            udph = (struct udphdr *) ((u32 *) ip_hdr + ip_hdr->ihl);
            *h_len = 8;
            *payload_length = ntohs(udph->len) - (*h_len);
            break;
    }
}

void *push_udp(struct iphdr *ip_hdr,
        const struct sk_buff *skb,
        struct sk_buff *skb2, int payload_offset, int payload_length, int udph_len)
{
    void *ptr;
    char *payload;
    struct udphdr *udph;
    pr_debug("In push_udp\n");
    udph = (struct udphdr *) ((u32 *) ip_hdr + ip_hdr->ihl);
    payload = (char *) udph + udph_len;
    if (!(ptr = skb_put(skb2, payload_length))) {
        return ptr;
    }
    // memcpy(ptr, payload, payload_length);
    skb_copy_bits(skb, payload_offset, ptr, payload_length);
    if (!(ptr = skb_push(skb2, udph_len))) {
        return ptr;
    }
    skb_reset_transport_header(skb2);
    memcpy(ptr, udph, udph_len);
    return ptr;
}

void *push_tcp(struct iphdr *ip_hdr,
        const struct sk_buff *skb,
        struct sk_buff *skb2, int payload_length, int tcph_len)
{
    void *ptr;
    char *payload;
    struct tcphdr *tcph;
    pr_debug("In push_tcp\n");
    tcph = (struct tcphdr *) ((u32 *) ip_hdr + ip_hdr->ihl);
    payload = (char *) tcph + tcph_len;
    if (!(ptr = skb_put(skb2, payload_length))) {
        return ptr;
    }
    memcpy(ptr, payload, payload_length);
    if (!(ptr = skb_push(skb2, tcph_len))) {
        return ptr;
    }
    skb_reset_transport_header(skb2);
    memcpy(ptr, tcph, tcph_len);
    return ptr;
}


/* Get eth_addr for a dst_ip by looking up into the ARP table */
int get_dst_haddr(unsigned char* daddr, u32 dst_ip, struct net_device *dev){
    struct neighbour *neigh = neigh_lookup(&arp_tbl, &dst_ip, dev);
    int err = -ENXIO;
    if(neigh){
        read_lock_bh(&neigh->lock);
        memcpy(daddr, neigh->ha, dev->addr_len);
        read_unlock_bh(&neigh->lock);
        neigh_release(neigh);
        err = 0;
    }
    else{
        pr_debug("neigh_lookup failed!\n");
    }
    return err;
}

void print_payload(char* buff, u32 len){
    u32 i = 0;
    while(i < len){
        pr_debug("%0x %c  ", buff[i], buff[i]);
        i = i+1;
    }
}

void ip_to_str(unsigned int ip, char *str)
{
    sprintf(str, "%u.%u.%u.%u", (ip&0xFF000000)>>24, (ip&0xFF0000)>>16, (ip&0xFF00)>>8, ip&0xFF);
}

void print_ip(u32 ip){
    char ip_str[INET_ADDRSTRLEN];
    ip_to_str(ntohl(ip), ip_str);
    pr_debug("%s", ip_str);
}

void print_mac(unsigned char * mac) {
    pr_debug("mac address: %02x:%02x:%02x:%02x:%02x:%02x \n",mac[0],mac[1],mac[2],mac[3],mac[4],mac[5]);
}


