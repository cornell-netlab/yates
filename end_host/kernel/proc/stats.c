#include "stats.h"
static DEFINE_SPINLOCK(stats_lock);
static DEFINE_HASHTABLE(stats_table, 5);

int display_table(struct seq_file *m, void *v) {
    struct stats_entry *ste;
    int i;
    seq_printf(m, "Dst IP\tBytes\tmsecs\n");
    hash_for_each_rcu(stats_table, i, ste, hash_node){
        seq_printf(m, "%u\t%lu\t%u\n", ste->dst_ip, ste->bytes, jiffies_to_msecs(jiffies));
    }
    return 0;
}

int open_stats_proc(struct inode *inode, struct  file *file) {
    return single_open(file, display_table, NULL);
}

const struct file_operations proc_file_fops = {
    .owner = THIS_MODULE,
    .open = open_stats_proc,
    .llseek = seq_lseek,
    .release = single_release,
    .read  = seq_read,
};

void create_new_stats_proc_entry(struct proc_dir_entry * proc_entry) {
    proc_entry = proc_create(YATES_STATS,0666,NULL, &proc_file_fops);
    if(!proc_entry)
    {
        pr_debug("Error creating proc entry");
    }
    else
    {
        pr_debug("proc initialized");
    }
}

void delete_stats_proc_entry(void) {
    pr_debug("Deleting stats proc entry\n");
    remove_proc_entry(YATES_STATS, NULL);
}

void stats_entry_del(struct stats_entry *ste){
    unsigned long flags;
    spin_lock_irqsave(&stats_lock, flags);
    hash_del_rcu(&ste->hash_node);
    spin_unlock_irqrestore(&stats_lock, flags);
}

void stats_entry_add(struct stats_entry *ste){
    unsigned long flags;
    u32 key = hash_ip(ste->dst_ip);
    ste->hash_key = key;
    spin_lock_irqsave(&stats_lock, flags);
    hash_add_rcu(stats_table,
            &ste->hash_node,
            key);
    spin_unlock_irqrestore(&stats_lock, flags);
}

struct stats_entry *stats_entry_get(u32 dst_ip){
    u32 key;
    struct stats_entry *ste;
    struct stats_entry *ret = NULL;

    key = hash_ip(dst_ip);
    hash_for_each_possible_rcu(stats_table, ste, hash_node, key){
        if(ste->dst_ip == dst_ip){
            ret = ste;
            break;
        }
    }
    return ret;
}

int stats_entry_inc(u32 dst_ip, long len){
    u32 key;
    struct stats_entry *ste;
    key = hash_ip(dst_ip);
    pr_debug("ip:%u len:%ld", dst_ip, len);
    hash_for_each_possible_rcu(stats_table, ste, hash_node, key){
        if(ste->dst_ip == dst_ip){
            ste->bytes += len;
            return 0;
        }
    }
    ste = kmalloc(sizeof(*ste), GFP_ATOMIC);
    if (!ste){
        return -1;
    }
    ste->dst_ip = dst_ip;
    ste->bytes = len;
    stats_entry_add(ste);
    return 0;
}
