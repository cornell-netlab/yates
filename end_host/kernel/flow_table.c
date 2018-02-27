#include "flow_table.h"

static DEFINE_SPINLOCK(ft_mutex);

u64 flow_idle_time (ulong last_used_jiffies){
    u64 idle_time;
    idle_time = jiffies_to_msecs(jiffies - last_used_jiffies);
    return idle_time;
}

bool flow_key_equal (struct flow_keys k1, struct flow_keys k2){
    if(k1.src == k2.src && k1.dst == k2.dst && k1.ports == k2.ports){
        return true;
    }

    return false;
}

struct stack get_random_stack_for_dst(u32 dst_ip, routing_table_t* routing_table) {
    struct stack_list stks;
    struct stack empty_stack;
    u32 n, total_wt = 0, i;

    empty_stack.num_tags = 0;
    empty_stack.tags = NULL;

    stks = routing_table_get(routing_table, dst_ip);
    if(stks.num_stacks <= 0){
        pr_debug("routing table miss. Returning empty_stack for (%u)", dst_ip);
        return empty_stack;
    }
    else{
        pr_debug("routing table: hit\n");
    }

    for (i=0; i< stks.num_stacks; i++){
        total_wt += stks.stacks[i].weight;
    }

    get_random_bytes(&n, sizeof(u32));
    n = n % total_wt;

    for(i=0; i<stks.num_stacks; i++){
        if(n < stks.stacks[i].weight){
            break;
        }
        n -= stks.stacks[i].weight;
    }

    pr_debug("exit get_random_stack_for_dst: %d %d / %d \n", n, i,
            stks.num_stacks);
    return stks.stacks[i];
}

match_stk_t *flow_table_new_match_stk(struct flow_keys match,
        struct stack stk) {
    match_stk_t *new_match_stk;

    if((new_match_stk = kmalloc(sizeof(match_stk_t), GFP_ATOMIC)) == NULL) {
        return NULL;
    }

    new_match_stk->match = match;
    new_match_stk->stk = stk;
    new_match_stk->last_used = jiffies;
    new_match_stk->next = NULL;

    return new_match_stk;
}

void free_match_stk_entry(match_stk_t* entry){
    if(entry != NULL){
        stack_free(entry->stk);
        kfree(entry);
    }
}

void flow_table_set( flow_table_t *ft, struct flow_keys match,
        struct stack orig_stk ) {
    unsigned long flags = 0;
    int index = 0;
    match_stk_t *new_match_stk = NULL;
    match_stk_t *curr = NULL;
    match_stk_t *prev = NULL;
    match_stk_t *tmp = NULL;
    match_stk_t *to_free = NULL;

    // keep separate copies of stack in routing and flow tables
    struct stack stk = stack_dup(orig_stk);

    new_match_stk = flow_table_new_match_stk(match, stk);
    if(new_match_stk == NULL){
        stack_free(stk);
        return;
    }
    index = flow_keys_hash(match) % ft->size;

    pr_debug("FT: setting flow entry %d ... num_flows: %d -> %d\n", index, 
            atomic_read(&ft->num_flows), atomic_read(&ft->num_flows)+1);

    spin_lock_irqsave(&ft_mutex, flags);
    curr = rcu_dereference_bh(ft->table[index]);

    while(curr != NULL && !flow_key_equal(match, curr->match)) {
        // ---
        if(flow_idle_time(curr->last_used) > IDLE_TIMEOUT && 0){
            // if idle timed-out, remove flow entry
            pr_debug("FT: non-matching flow timeout remove it %d -> %d flows\n",
                    atomic_read(&ft->num_flows), 
                    atomic_read(&ft->num_flows)-1);
            if(prev == NULL) { 
                // at the beginning of bucket
                tmp = curr->next;
                rcu_assign_pointer(ft->table[index], tmp); 
                curr->next = to_free;
                to_free = curr;
                curr = rcu_dereference_bh(ft->table[index]);
            }
            else { 
                // otherwise
                prev->next = curr->next;
                curr->next = to_free;
                to_free = curr;
                curr = rcu_dereference_bh(prev->next);
            }
            atomic_dec(&ft->num_flows);
        }
        else {
            // Else advance to next entry in bucket
            pr_debug("FT: active non-matching flow\n");
            prev = curr;
            curr = rcu_dereference_bh(curr->next);
        }
    }

    if(curr != NULL && flow_key_equal(match, curr->match)) {
        curr->stk = stk;
        free_match_stk_entry(new_match_stk);
    }
    else {
        tmp = rcu_dereference_bh(ft->table[index]);
        if(curr == tmp) {
          pr_debug("FT: creating new bucket entry at %d\n", index);
          new_match_stk->next = curr;
          rcu_assign_pointer(ft->table[index], new_match_stk);
        } 
        else if (curr == NULL) {
          pr_debug("FT: appending bucket entry at %d\n", index);
          rcu_assign_pointer(prev->next, new_match_stk);
        } 
        else {
          pr_debug("FT: inserting bucket entry at %d\n", index);
          new_match_stk->next = curr;
          rcu_assign_pointer(prev->next, new_match_stk);
        }
       atomic_inc(&ft->num_flows);
    }
    spin_unlock_irqrestore(&ft_mutex, flags);

}


void flow_table_rem(flow_table_t *ft, struct flow_keys match) {
    int index = 0;
    unsigned long flags = 0;
    match_stk_t *curr = NULL;
    match_stk_t *last = NULL;

    index = flow_keys_hash(match) % ft->size;

    curr = rcu_dereference_bh(ft->table[index]);

    pr_debug("FT: removing bucket entry at %d\n", index);
    while(curr != NULL && !flow_key_equal(match, curr->match)) {
        last = curr;
        curr = rcu_dereference_bh(curr->next);
    }

    if(curr != NULL && flow_key_equal(match, curr->match)) {
        // entry exists
        spin_lock_irqsave(&ft_mutex, flags);
        if(curr == ft->table[index]){
            // if at the beginning of bucket
            rcu_assign_pointer(ft->table[index], curr->next);
        } else {
            // otherwise
            rcu_assign_pointer(last->next, curr->next);
        }
        spin_unlock_irqrestore(&ft_mutex, flags);
        // free both stk and entry
        free_match_stk_entry(curr);
        atomic_dec(&ft->num_flows);
    }
    // else entry doesn't exist
}



struct stack flow_table_get( flow_table_t *ft, struct flow_keys match, routing_table_t* routing_table, u32 dst_ip) {
    struct stack no_stack;
    struct stack new_stk;
    struct stack old_stk;
    unsigned long flags = 0;
    match_stk_t *curr = NULL;
    match_stk_t *prev = NULL;
    int index = 0;
    no_stack.num_tags = -1;
    index = flow_keys_hash(match) % ft->size;

    curr = rcu_dereference_bh(ft->table[index]);
    
    while(curr != NULL && !flow_key_equal(match, curr->match)) {
        pr_debug("FT: active non-matching flow\n");
        prev = curr;
        curr = rcu_dereference_bh(curr->next);
    }

    // flow_table returns "no_stack" on miss
    if(curr == NULL || !flow_key_equal(match, curr->match)) {
        pr_debug("FT: no matching flow\n");
        return no_stack;
    }

    // found a matching stack - check idle timeout before returning
    if(flow_idle_time(curr->last_used) > IDLE_TIMEOUT){
        pr_debug("FT: matched flow - timeout.. num_flows %d -> %d\n", 
                atomic_read(&ft->num_flows), atomic_read(&ft->num_flows) - 1);
        // if idle timed-out, remove flow entry and return no_stack
        new_stk = stack_dup(get_random_stack_for_dst(dst_ip, routing_table));
        old_stk = curr->stk;
        spin_lock_irqsave(&ft_mutex, flags);
        curr->stk = new_stk;
        spin_unlock_irqrestore(&ft_mutex, flags);
        stack_free(old_stk);
    }

    pr_debug("FT: matched flow - updating last_used\n");
    // update the last_used value for successful match
    curr->last_used = jiffies;
    return curr->stk;
}

flow_table_t *flow_table_create( int size ) {
    flow_table_t *ft = NULL;
    int i;

    if (size < 1){
        return NULL;
    }

    if ((ft = kmalloc(sizeof(flow_table_t), GFP_ATOMIC)) == NULL) {
        return NULL;
    }

    if ((ft->table = kmalloc(sizeof(match_stk_t *) * size, GFP_ATOMIC))
            == NULL) {
        return NULL;
    }

    for (i = 0; i < size; i++) {
        ft->table[i] = NULL;
    }

    ft->size = size;
    atomic_set(&ft->num_flows, 0);
    return ft;
}

void flow_table_delete(flow_table_t *ft){
    int i;

    match_stk_t *match_stk_entry;
    match_stk_t *next;

    for(i=0; i<ft->size; i++){
        match_stk_entry = ft->table[i];
        while (match_stk_entry != NULL){
            next = match_stk_entry->next;
            // entire stack was copied into flow table on lookup; 
            // stack_free the copy here
            stack_free(match_stk_entry->stk);
            // free the entry
            kfree(match_stk_entry);
            match_stk_entry = next;
        }
    }
    kfree(ft->table);
}
