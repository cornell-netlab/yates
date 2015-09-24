#pragma once

#include <linux/kernel.h>
#include <linux/module.h>
#include <linux/net.h>
#include <net/sock.h>
#include <net/flow_keys.h>
#include <linux/slab.h>
#include <linux/jhash.h>
#include <linux/jiffies.h>
#include <linux/spinlock.h>
#include "stack.h"
#include "routing_table.h"

#define DEFAULT_FLOW_TABLE_SIZE 40960
#define IDLE_TIMEOUT 1000

struct match_stk_s {
    struct flow_keys match;
    struct stack stk;
    unsigned long last_used;
    struct match_stk_s __rcu *next;
};

typedef struct match_stk_s match_stk_t;

struct flow_table_s {
    int size;
    struct match_stk_s **table;
    atomic_t num_flows; 
};

static u32 hashrnd __read_mostly;

static __always_inline void __flow_hash_secret_init(void)
{
    net_get_random_once(&hashrnd, sizeof(hashrnd));
}

static __always_inline u32 __flow_hash_3words(u32 a, u32 b, u32 c)
{
    __flow_hash_secret_init();
    return jhash_3words(a, b, c, hashrnd);
}

static __always_inline u32 flow_keys_hash(struct flow_keys flow_key){
    return __flow_hash_3words((__force u32) flow_key.src,
                (__force u32) flow_key.dst,
                (__force u32) flow_key.ports);
}

typedef struct flow_table_s flow_table_t;

flow_table_t *flow_table_create( int size );

void flow_table_delete(flow_table_t *);

void flow_table_set( flow_table_t *hashtable, struct flow_keys match, struct stack stk); 

struct stack flow_table_get( flow_table_t *hashtable, struct flow_keys match, routing_table_t* routing_table, u32 dst_ip);

void flow_table_rem( flow_table_t *hashtable, struct flow_keys match );

struct stack get_random_stack_for_dst(u32 dst_ip, routing_table_t* routing_table);
