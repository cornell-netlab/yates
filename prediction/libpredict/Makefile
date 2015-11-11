THISDIR := $(realpath .)

#EXECUTABLE = predict
LIBRARY = libpredict.so

CPPFLAGS  =
INCLUDES  = \
 -I /usr/local/include/opencv2 

LDFLAGS   = \
 -L /usr/local/lib \
 -lopencv_core \
 -lopencv_highgui

ifeq (($shell uname), Linux)
LDFLAGS   += \
 -Wl,-rpath,/usr/local/lib 
endif


include ../Makefile.lib

