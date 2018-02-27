#include <linux/ip.h>
#include <linux/udp.h>
#include <linux/tcp.h>
#include <linux/inet.h>
#include <net/tcp.h>

static inline void set_src_ip (struct iphdr* iph, int src_ip){
    iph->saddr = src_ip;
}

__wsum fix_csum(struct iphdr* iph);
