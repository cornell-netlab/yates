THISDIR := $(realpath .)

EXECUTABLE = predict

CPPFLAGS  =
INCLUDES  = \
 -I /usr/local/include/opencv2 

LDFLAGS   = \
 -L /usr/local/lib \
 -lopencv_core \
 -lopencv_highgui \
 -lboost_filesystem \
 -lboost_system \
 -lboost_program_options

include ../Makefile.exe

