#pragma once

#include <linux/kernel.h>
#include <linux/module.h>
#include <linux/skbuff.h>
#include <linux/ip.h>
#include <linux/inet.h>
#include <net/dsfield.h>

bool check_valid_dscp(u_int8_t tos);

bool set_dsfield(struct sk_buff* skb, u_int8_t tos);
 
