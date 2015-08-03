#pragma once

#include <linux/kernel.h>
#include <linux/module.h>
#include <linux/net.h>
#include <linux/slab.h>
#include <linux/spinlock.h>
#include "stack.h"

struct ip_stks_s {
    u32 ip;
    struct stack_list stks;
    struct ip_stks_s *next;
};

typedef struct ip_stks_s ip_stks_t;

struct routing_table_s {
    int size;
    struct ip_stks_s **table;
};

typedef struct routing_table_s routing_table_t;

routing_table_t *routing_table_create( int size );

void routing_table_reset(routing_table_t *, int );

void routing_table_delete(routing_table_t *);

void routing_table_set( routing_table_t *rt, u32 ip, struct stack_list stks); 

struct stack_list routing_table_get( routing_table_t *rt, u32 ip );
