#pragma once

#include <linux/kernel.h>
#include <linux/module.h>
#include <asm/uaccess.h>
#include <linux/cdev.h>
#include <linux/proc_fs.h>
#include <linux/seq_file.h>
#include <linux/string.h>

#include "stack.h"
#include "routing_table.h"

#define MAX_PROC_SIZE 40960
#define KULFI_PROC "kulfi"

extern char proc_data[MAX_PROC_SIZE];

extern routing_table_t * routing_table;

extern routing_table_t * tmp_routing_table;

ssize_t read_proc(struct file *file, char *buf, size_t count, loff_t *data);

ssize_t write_proc(struct file *file,const char *buf,size_t count, loff_t *data );

int display_table(struct seq_file *m, void *v);

int open_proc(struct inode *inode, struct  file *file);

void create_new_proc_entry(struct proc_dir_entry *);

void delete_proc_entry(void);

