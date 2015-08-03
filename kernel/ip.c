#include "ip.h"

__wsum fix_csum(struct iphdr* iph){
    struct tcphdr * tcp_hdr;
    struct udphdr * udp_hdr;
    __wsum skb_csum = 0;
    int ip_pkt_len;

    ip_pkt_len = ntohs(iph->tot_len);
    // Calculate checksum
    if (iph->protocol == IPPROTO_UDP){
        pr_debug("UDP pkt\n");
        udp_hdr = (struct udphdr *) ((u32 *) iph + iph->ihl);
        udp_hdr->check = 0;
        skb_csum = csum_partial(udp_hdr, ip_pkt_len - 4 * iph->ihl, 0);
        udp_hdr->check =csum_tcpudp_magic(iph->saddr, iph->daddr, ip_pkt_len - 4 * iph->ihl,
                iph->protocol, skb_csum); 
    }
    else if(iph->protocol == IPPROTO_TCP){
        pr_debug("TCP pkt\n");
        tcp_hdr = (struct tcphdr *) ((u32 *) iph + iph->ihl);
        tcp_hdr->check=0;
        skb_csum = csum_partial(tcp_hdr, ip_pkt_len - 4 * iph->ihl, 0);
        tcp_hdr->check = tcp_v4_check(ip_pkt_len - 4 * iph->ihl, iph->saddr, iph->daddr,
                csum_partial(tcp_hdr, ip_pkt_len - 4 * iph->ihl, 0));
    }
    else {
        pr_debug(" not (UDP or TCP) pkt %04x\n", iph->protocol);
        skb_csum = 0;
    }
    iph->check = 0;
    iph->check = ip_fast_csum((unsigned char*)iph, iph->ihl);

    return skb_csum;
}
