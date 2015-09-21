THISDIR := $(realpath .)

EXECUTABLE = generate

CPPFLAGS  =
INCLUDES  = \
 -I $(THISDIR)/../libpredict/include \
 -I /usr/local/include/opencv2 

LDFLAGS   = \
 -L /usr/local/lib \
 -lopencv_core \
 -lopencv_highgui \
 -lboost_filesystem-mt \
 -lboost_system-mt \
 -lboost_program_options

include ../Makefile.exe

