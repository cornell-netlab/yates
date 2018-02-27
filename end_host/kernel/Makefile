SHELL := /bin/bash
CC=gcc
UNAME := $(shell uname)
PWD := $(shell pwd)
KDIR=/lib/modules/$(shell uname -r)/build
SRCS=dsfield.c\
	 flow_table.c\
	 helper.c\
	 ip.c\
	 routing_table.c\
	 stack.c\
	 vlan.c\
	 proc/routes.c\
	 proc/stats.c\
	 yates_mod.c

OBJS = $(SRCS:.c=.o)
#CFLAGS_yates_mod.o := -DDEBUG
#CFLAGS_routes.o := -DDEBUG

obj-m += modyates.o
modyates-objs += $(OBJS) yates_mod.o

all: 
	@echo target=$@ ccflags-y=${ccflags-y} CFLAGS=${CFLAGS}
	$(MAKE) -C $(KDIR) M=$(PWD) modules

clean:
	make -C $(KDIR) M=$(PWD) clean

.c.o:
	${CC} ${CFLAGS} ${DEFS} $< -o $@  

reinstall: clean all
	@ if lsmod | grep "modyates" &> /dev/null ; then \
		sudo rmmod modyates; \
	fi
	sudo insmod modyates.ko
