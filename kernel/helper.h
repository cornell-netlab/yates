#include <linux/ip.h>
#include <linux/udp.h>
#include <linux/tcp.h>
#include <linux/inet.h>
#include <net/sock.h>
#include <net/arp.h>
#include <net/tcp.h>
#include <net/icmp.h>


void calc_transport_len(const struct sk_buff *skb,
        struct iphdr *ip_hdr, int *h_len, int *payload_length);

void *push_udp(struct iphdr *ip_hdr,
        const struct sk_buff *skb,
        struct sk_buff *skb2, int payload_offset, int payload_length, int udph_len);


void *push_tcp(struct iphdr *ip_hdr,
        const struct sk_buff *skb,
        struct sk_buff *skb2, int payload_length, int tcph_len);

/* Get eth_addr for a dst_ip by looking up into the ARP table */
int get_dst_haddr(unsigned char* daddr, u32 dst_ip, struct net_device *dev);

void print_payload(char* buff, u32 len);


void ip_to_str(unsigned int ip, char *str);

void print_ip(u32 ip);

void print_mac(unsigned char * mac); 


