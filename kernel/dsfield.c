#include "dsfield.h"

bool set_dsfield(struct sk_buff* skb, u_int8_t tos){
    u_int8_t orig_tos;
    struct iphdr* iph = (struct iphdr*)skb_network_header(skb);

    orig_tos = ipv4_get_dsfield(iph);
    if (tos != orig_tos){
        if (!skb_make_writable(skb, sizeof(struct iphdr))){
            printk(KERN_DEBUG "Failed to make skb writable\n");
            return false;
        }
        iph = (struct iphdr*)skb_network_header(skb);
        ipv4_change_dsfield(iph, 0, tos);
    }
    return true;
}

