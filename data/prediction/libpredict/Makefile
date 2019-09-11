THISDIR := $(realpath .)

#EXECUTABLE = predict
LIBRARY = libpredict.so

CPPFLAGS  =

ifeq ($(shell uname), Darwin)

INCLUDES  = \
 -isystem /opt/local/libcpp-software/include \
 -I /usr/local/include/opencv2 \
 -I /usr/local/include

LDFLAGS   = \
 -L /opt/local/libcpp-software/lib \
 -L /usr/local/lib 

endif


ifeq (($shell uname), Linux)
INCLUDES  = \
 -I /usr/local/include/opencv2

LDFLAGS   = \
 -L /usr/lib/x86_64-linux-gnu \
 -Wl,-rpath,/usr/local/lib
endif


include ../Makefile.library

