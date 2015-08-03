#include "stack.h"

void stack_free(struct stack stk) {
    if(stk.tags != NULL){
        kfree(stk.tags);
    }
}

void stack_alloc(struct stack *stk, int num_tags){
    stk->num_tags = num_tags;
    if(num_tags > 0){
        stk->tags = kmalloc(num_tags * sizeof(u_int16_t), GFP_ATOMIC);
    }
    else {
        stk->tags = NULL;
    }
}

void stack_list_free(struct stack_list stks) {
    int i;
    if(stks.stacks != NULL){
        for (i=0; i<stks.num_stacks; i++){
            stack_free(stks.stacks[i]);
        }
        kfree(stks.stacks);
    }
}

void stack_list_alloc(struct stack_list *stks, int num_stacks){
    stks->num_stacks = num_stacks;
    if(num_stacks > 0) {
        stks->stacks = kmalloc(num_stacks * sizeof(struct stack), GFP_ATOMIC);
    }
    else {
        stks->stacks = NULL;
    }
}

struct stack stack_dup(struct stack orig){
    int i;
    struct stack new;
    stack_alloc(&new, orig.num_tags);
    new.weight = orig.weight;
    for (i=0; i<orig.num_tags; i++){
        new.tags[i] = orig.tags[i];
    }
    return new;
}
