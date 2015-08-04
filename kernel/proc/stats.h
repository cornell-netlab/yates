#pragma once

#include <linux/kernel.h>
#include <linux/module.h>
#include <asm/uaccess.h>
#include <linux/cdev.h>
#include <linux/proc_fs.h>
#include <linux/seq_file.h>
#include <linux/string.h>
#include <linux/hashtable.h>
#include <linux/jhash.h>
#include <linux/net.h>
#include <linux/slab.h>

#define KULFI_STATS "kulfi_stats"

struct stats_entry {
    u32 dst_ip;
    u32 hash_key;
    ulong bytes;
    struct hlist_node hash_node;
};

static u32 hashrnd __read_mostly;

static __always_inline u32 hash_ip(u32 ip_addr)
{
    net_get_random_once(&hashrnd, sizeof(hashrnd));
    return jhash_1word(ip_addr, hashrnd);
}

int display_table(struct seq_file *m, void *v);

int open_proc_stats(struct inode *inode, struct  file *file);

void create_new_stats_proc_entry(struct proc_dir_entry *);

void delete_stats_proc_entry(void);

void stats_entry_del(struct stats_entry*);
void stats_entry_add(struct stats_entry*);
struct stats_entry *stats_entry_get(u32);

int stats_entry_inc(u32, long);
