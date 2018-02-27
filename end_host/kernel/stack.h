#pragma once

#include <linux/kernel.h>
#include <linux/module.h>
#include <linux/slab.h>

struct stack {
    int num_tags;
    int weight;
    u_int16_t *tags;
};

struct stack_list {
    int num_stacks;
    struct stack *stacks;
};

void stack_free(struct stack);
void stack_alloc(struct stack* stk, int num_tags);

void stack_list_free(struct stack_list);
void stack_list_alloc(struct stack_list *stks, int num_stacks);

// Duplicate a stack
struct stack stack_dup(struct stack orig);
