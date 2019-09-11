
THISDIR := $(realpath .)

UNAME    := $(shell uname)

EXECUTABLE = generate

CPPFLAGS  =

INCLUDES  = \
 -I $(THISDIR)/../libpredict/include \

ifeq ($(UNAME), Darwin)

INCLUDES  += \
 -isystem /opt/local/libcpp-software/include \
 -I /usr/local/include/opencv2 \
 -I /usr/local/include

LDFLAGS   = \
 -L /opt/local/libcpp-software/lib \
 -L /usr/local/lib \
 -L $(THISDIR)/../libpredict/bin \
 -lboost_filesystem-mt \
 -lboost_program_options-mt \
 -lboost_system-mt \
 -lpredict

endif

ifeq ($(UNAME), Linux)

INCLUDES  += \
 -I /usr/local/include/opencv2 \
 -I /usr/include 

LDFLAGS   = \
 -L /usr/lib/x86_64-linux-gnu \
 -L $(THISDIR)/../libpredict/bin \
 -Wl,-rpath,$(THISDIR)/../libpredict/bin \
 -lpredict \
 -lboost_filesystem \
 -lboost_program_options \
 -lboost_system

endif

include ../Makefile.exe

