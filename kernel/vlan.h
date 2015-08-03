#pragma once

#include <linux/kernel.h>
#include <linux/module.h>
#include <linux/skbuff.h>
#include <linux/ip.h>
#include <linux/inet.h>
#include <linux/if_ether.h>

#include "stack.h"

typedef struct vlan_label vlan_label;

struct vlan_label {
    u_int16_t label;
    u_int16_t type;
};

/* Set bits of VLAN header */
u_int32_t set_vlan_bits(u_int16_t label_val, bool inner_most_tag);

/* Push one vlan tag */
void set_vlan(struct sk_buff * skb, u_int16_t val, bool inner_most_tag);

/* Push a vlan stack (list of vlan tags) */
bool set_vlan_stack_static(struct sk_buff * skb, u_int16_t *tags, int stk_len);


/* Wrapper over set_vlan_stack_static to handle empty stacks */
bool set_vlan_stack(struct sk_buff * skb, struct stack* stk);

/*
int pop_vlan(struct sk_buff *skb);
int push_vlan(struct sk_buff *skb, u_int16_t vlan_id);
*/
