THISDIR := $(realpath .)

#EXECUTABLE = predict
LIBRARY = libpredict.so

CPPFLAGS  =
INCLUDES  = \
 -I /usr/local/include/opencv2 

LDFLAGS   = \
 -L /usr/local/lib

ifeq (($shell uname), Linux)
LDFLAGS   += \
 -Wl,-rpath,/usr/local/lib 
endif


include ../Makefile.library

