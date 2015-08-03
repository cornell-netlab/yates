#include "proc.h"

static DEFINE_SPINLOCK(rt_lock);

ssize_t read_proc(struct file *file, char *buf, size_t count, loff_t *data)
{
    int bytes_to_read;
    pr_debug("read_proc current proc_data: %s\n", proc_data);
    if(MAX_PROC_SIZE - *data > count) {
        bytes_to_read = count;
    }
    else {
        bytes_to_read = MAX_PROC_SIZE - *data;
    }

    if (bytes_to_read == 0){
        pr_debug("reached end of /proc/kulfi\n");
    }

    if(copy_to_user(buf, proc_data, bytes_to_read)){
        return -EFAULT;
    }

    *data += bytes_to_read;
    return bytes_to_read;
}

int get_int(char **buf){
    char *token;
    unsigned int x;
    int res;
    token  = strsep(buf, " ");
    if(token == NULL) {
        pr_debug("get_int: NULL token!\n");
        x = -1;
    }
    else {
        res = kstrtouint(token,10,&x);
        if(res == -ERANGE) {
            pr_debug("kstrtoint failed: ERANGE at |%s|!\n", token);
            x = -1;
        } else if (res == -EINVAL) {
            pr_debug("kstrtoint failed: EINVAL at |%s|!\n", token);
            x = -1;
        }
    }
    return x;
}

void process_proc(void){
    unsigned int num_dsts, n_stks, n_tags, ip, tag, i, j, weight;
    char *dup;
    routing_table_t * new_routing_table = NULL;
    routing_table_t * old_routing_table = NULL;
    pr_debug("Parsing proc data");
    dup = kstrdup(proc_data, GFP_ATOMIC);
    num_dsts = get_int(&dup);
    if(num_dsts <= 0) {
        return;
    }
    pr_debug("Num routes: [%d] \nCreating new routing_table and flow_table\n", num_dsts);

    new_routing_table = routing_table_create(num_dsts);
    // TODO: 1. Reset flow table so that "flow table miss" happens for all "new" pkts
    // and updated routing_table is looked up
    // or 2. do not flush flow_table. wait for idle time out of flows
    // flow_table_delete(flow_table);
    // flow_table = flow_table_create(DEFAULT_FLOW_TABLE_SIZE);

    spin_lock(&rt_lock);
    old_routing_table = routing_table;
    // for all destination address
    while(num_dsts-->0){

        // create stack_list
        struct stack_list stk_list;
        ip = get_int(&dup);
        pr_debug("IP: %u", ip);
        n_stks = get_int(&dup);
        pr_debug("#stacks: %d", n_stks);

        stack_list_alloc(&stk_list, n_stks);
        i = 0;
        while(i<n_stks) {
            // create stacks
            struct stack stk;
            weight = get_int(&dup);
            n_tags=get_int(&dup);
            pr_debug("weight = %d  #tags: %d :\n", weight, n_tags);
            stack_alloc(&stk, n_tags);
            stk.weight = weight;
            j = 0;
            while(j < n_tags){
                tag = get_int(&dup);
                pr_debug(" %d\n", tag);
                stk.tags[j] = tag;
                j++;
            }
            stk_list.stacks[i] = stk;
            i++;
        }
        // Add entries to new routing_table
        pr_debug("Updating routing table...\n");
        routing_table_set(new_routing_table, ip, stk_list);
    }

    // Update routing table; 
    rcu_assign_pointer(routing_table, new_routing_table);
    spin_unlock(&rt_lock);
    synchronize_rcu();

    if(old_routing_table != NULL){
        pr_debug("Deleting old_routing_table\n");
        routing_table_delete(old_routing_table);
    }

    pr_debug("End of data\n");
}

ssize_t write_proc(struct file *file, const char *buf, size_t count, loff_t *data)
{
    pr_debug("write_proc\n");
    memset(proc_data, '\0', sizeof(char) * MAX_PROC_SIZE);
    if(count > MAX_PROC_SIZE)
        count = MAX_PROC_SIZE;
    if(copy_from_user(proc_data, buf, count))
        return -EFAULT;

    pr_debug("proc_data updated to %s\n", proc_data);
    process_proc();
    return count;
}

int display_table(struct seq_file *m, void *v) {
    seq_printf(m, "MAC\tStacks\n");
    seq_printf(m, "===\t======\n");
    return 0;
}

int open_proc(struct inode *inode, struct  file *file) {
    return single_open(file, display_table, NULL);
}

const struct file_operations proc_file_fops = {
    .owner = THIS_MODULE,
    .open = open_proc,
    .llseek = seq_lseek,
    .release = seq_release,
    .read  = read_proc,
    .write = write_proc,
};

void create_new_proc_entry(struct proc_dir_entry * proc_entry) {
    proc_entry = proc_create(KULFI_PROC,0666,NULL, &proc_file_fops);
    if(!proc_entry)
    {
        pr_debug("Error creating proc entry");
    }
    else
    {
        pr_debug("proc initialized");
    }
}

void delete_proc_entry(void) {
    pr_debug("Deleting proc entry\n");
    remove_proc_entry(KULFI_PROC, NULL);
}
