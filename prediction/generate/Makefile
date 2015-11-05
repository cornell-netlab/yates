THISDIR := $(realpath .)

EXECUTABLE = generate

CPPFLAGS  =
INCLUDES  = \
 -I $(THISDIR)/../libpredict/include \
 -I /usr/local/include/opencv2 \
 -I /usr/include 

LDFLAGS   = \
 -L /usr/lib/x86_64-linux-gnu \
 -L /usr/local/lib \
 -lopencv_core \
 -lopencv_highgui \
 -lboost_filesystem \
 -lboost_program_options \
 -lboost_system 


include ../Makefile.exe

