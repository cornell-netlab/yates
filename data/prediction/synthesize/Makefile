
THISDIR := $(realpath .)

UNAME    := $(shell uname)

EXECUTABLE = synthesize

CPPFLAGS  =

INCLUDES  = \
 -I $(THISDIR)/../libpredict/include \

ifeq ($(shell uname), Darwin)

INCLUDES  += \
 -isystem /opt/local/libcpp-software/include \
 -I /usr/local/include/opencv2 \
 -I /usr/local/include

LDFLAGS   = \
 -L /opt/local/libcpp-software/lib \
 -L $(THISDIR)/../libpredict/bin \
 -lboost_filesystem-mt \
 -lboost_program_options-mt \
 -lboost_system-mt \
 -lpredict

endif

ifeq ($(shell uname), Linux)

INCLUDES  += \
 -I /usr/local/include/opencv2 \
 -I /usr/include 

LDFLAGS   = \
 -L /usr/lib/x86_64-linux-gnu \
 -L $(THISDIR)/../libpredict/bin \
 -Wl,-rpath,$(THISDIR)/../libpredict/bin \
 -lboost_filesystem \
 -lboost_program_options \
 -lboost_system \
 -lpredict

endif

include ../Makefile.exe

