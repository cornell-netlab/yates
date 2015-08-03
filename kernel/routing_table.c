#include "routing_table.h"

bool ip_equal (u32 ip_1, u32 ip_2) {
    return (ip_1 == ip_2);
}

ip_stks_t *routing_table_new_ip_stks_entry(u32 ip, struct stack_list stks) {
    ip_stks_t *new_ip_stks_entry;

    if((new_ip_stks_entry = kmalloc(sizeof(ip_stks_t), GFP_ATOMIC)) == NULL) {
        return NULL;
    }

    new_ip_stks_entry->ip = ip;
    new_ip_stks_entry->stks = stks;
    new_ip_stks_entry->next = NULL;

    return new_ip_stks_entry;
}

void routing_table_set( routing_table_t *rt, u32 ip, struct stack_list stks ) {
    int index = 0;
    ip_stks_t *new_ip_stks_entry = NULL;
    ip_stks_t *next = NULL;
    ip_stks_t *last = NULL;

    if(rt == NULL){
        return;
    }

    index = ip % rt->size;

    next = rt->table[index];

    while(next != NULL && !ip_equal(ip, next->ip)) {
        last = next;
        next = next->next;
    }

    if(next != NULL && ip_equal(ip, next->ip)) {
        next->stks = stks;
    } else {
        new_ip_stks_entry = routing_table_new_ip_stks_entry(ip, stks);

        if(next == rt->table[index]) {
            new_ip_stks_entry->next = next;
            rt->table[index] = new_ip_stks_entry;
        } else if (next == NULL) {
            last->next = new_ip_stks_entry;
        } else {
            new_ip_stks_entry->next = next;
            last->next = new_ip_stks_entry;
        }
    }
}

struct stack_list routing_table_get( routing_table_t *rt, u32 ip ) {
    int index = 0;
    ip_stks_t *ip_stks_entry;

    if(rt == NULL){
        struct stack_list empty_stacks;
        empty_stacks.num_stacks = 0;
        empty_stacks.stacks = NULL;
        return empty_stacks;
    }

    index = ip % rt->size;

    rcu_read_lock();
    ip_stks_entry = rcu_dereference_bh(rt->table[index]);
    while(ip_stks_entry != NULL && !ip_equal(ip, ip_stks_entry->ip)) {
        ip_stks_entry = rcu_dereference_bh(ip_stks_entry->next);
    }

    // routing_table returns "empty_stack" on miss - cache in flow_table to avoid double lookup
    if(ip_stks_entry == NULL || !ip_equal(ip, ip_stks_entry->ip)) {
        struct stack_list empty_stacks;
        empty_stacks.num_stacks = 0;
        empty_stacks.stacks = NULL;
        rcu_read_unlock();
        return empty_stacks;
    } 
    rcu_read_unlock();
    return ip_stks_entry->stks;
}

routing_table_t *routing_table_create( int size ) {
    routing_table_t *rt = NULL;
    int i;

    if(size < 1) return NULL;

    if((rt = kmalloc(sizeof(routing_table_t), GFP_ATOMIC)) == NULL) {
        return NULL;
    }

    if( ( rt->table = kmalloc(sizeof(ip_stks_t *) * size, GFP_ATOMIC)) == NULL ) {
        return NULL;
    }

    for (i = 0; i < size; i++) {
        rt->table[i] = NULL;
    }

    rt->size = size;
    return rt;
}

void routing_table_reset(routing_table_t *rt, int size){
    int i;
    routing_table_delete(rt);
    if( ( rt->table = kmalloc(sizeof(ip_stks_t *) * size, GFP_ATOMIC)) == NULL ) {
        pr_debug("Failed to allocate memory");
    }

    for (i = 0; i < size; i++) {
        rt->table[i] = NULL;
    }

    rt->size = size;
}

void routing_table_delete(routing_table_t *rt){
    int i;
    ip_stks_t *ip_stks_entry;
    ip_stks_t *next;

    if(rt != NULL){
        for(i=0; i<rt->size; i++){
            ip_stks_entry = rt->table[i];
            while(ip_stks_entry != NULL){
                next = ip_stks_entry->next;
                // free stacks in stack_list for a dst ip
                stack_list_free(ip_stks_entry->stks);
                kfree(ip_stks_entry);
                ip_stks_entry = next;
            }
        }
        kfree(rt->table);
    }
}
