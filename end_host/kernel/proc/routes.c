#include "routes.h"

static DEFINE_SPINLOCK(rt_lock);

ssize_t read_proc_routes(struct file *file, char *buf, size_t count, loff_t *data)
{
    int bytes_to_read;
    pr_debug("read_proc_routes current proc_routes_data: %s\n", proc_routes_data);
    if(MAX_ROUTES_SIZE - *data > count) {
        bytes_to_read = count;
    }
    else {
        bytes_to_read = MAX_ROUTES_SIZE - *data;
    }

    if (bytes_to_read == 0){
        pr_debug("reached end of /proc/yates\n");
    }

    if(copy_to_user(buf, proc_routes_data, bytes_to_read)){
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
    unsigned long flags;
    char *dup;
    routing_table_t * new_routing_table = NULL;
    routing_table_t * old_routing_table = NULL;
    pr_debug("Parsing proc data");
    dup = kstrdup(proc_routes_data, GFP_ATOMIC);
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

    spin_lock_irqsave(&rt_lock, flags);
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
    spin_unlock_irqrestore(&rt_lock, flags);

    if(old_routing_table != NULL){
        pr_debug("Deleting old_routing_table\n");
        routing_table_delete(old_routing_table);
    }

    pr_debug("End of data\n");
}

ssize_t write_proc_routes(struct file *file, const char *buf, size_t count, loff_t *data)
{
    pr_debug("write_proc_routes\n");
    memset(proc_routes_data, '\0', sizeof(char) * MAX_ROUTES_SIZE);
    if(count > MAX_ROUTES_SIZE)
        count = MAX_ROUTES_SIZE;
    if(copy_from_user(proc_routes_data, buf, count))
        return -EFAULT;

    pr_debug("proc_routes_data updated to %s\n", proc_routes_data);
    process_proc();
    return count;
}

const struct file_operations routes_file_fops = {
    .owner = THIS_MODULE,
    .open = open_proc_routes,
    .llseek = seq_lseek,
    .release = seq_release,
    .read  = read_proc_routes,
    .write = write_proc_routes,
};

int show_routes_proc(struct seq_file *m, void *v){
    return 0;
}

int open_proc_routes(struct inode *inode, struct file *file){
    return single_open(file, show_routes_proc, NULL);
}

void create_new_routes_proc_entry(struct proc_dir_entry * proc_routes_entry) {
    proc_routes_entry = proc_create(YATES_PROC,0666,NULL, &routes_file_fops);
    if(!proc_routes_entry)
    {
        pr_debug("Error creating routes proc entry");
    }
    else
    {
        pr_debug("routes proc initialized");
    }
}

void delete_routes_proc_entry(void) {
    pr_debug("Deleting routes proc entry\n");
    remove_proc_entry(YATES_PROC, NULL);
}
