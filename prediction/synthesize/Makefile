THISDIR := $(realpath .)

EXECUTABLE = synthesize

CPPFLAGS  =
INCLUDES  = \
 -I $(THISDIR)/../libpredict/include \
 -I /usr/local/include/opencv2 

LDFLAGS   = \
 -L /usr/local/lib \
 -L /usr/lib/x86_64-linux-gnu/ \
 -lopencv_core \
 -lopencv_highgui \
 -lboost_filesystem \
 -lboost_system \
 -lboost_program_options

include ../Makefile.exe

