THISDIR := $(realpath .)

EXECUTABLE = generate

CPPFLAGS  =
INCLUDES  = \
 -I $(THISDIR)/../libpredict/include \
 -I /usr/local/include/opencv2 \
 -I /usr/local/include

LDFLAGS   = \
 -L /usr/local/lib \
 -lopencv_core \
 -lopencv_highgui \
 -lboost_filesystem \
 -lboost_system \
 -lboost_program_options

include ../Makefile.exe

