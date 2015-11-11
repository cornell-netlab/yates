
THISDIR := $(realpath .)

UNAME    := $(shell uname)

EXECUTABLE = generate

CPPFLAGS  =

INCLUDES  = \
 -I $(THISDIR)/../libpredict/include \

ifeq ($(UNAME), Darwin)

INCLUDES  += \
 -I /usr/local/include/opencv2 \
 -I /usr/local/include

LDFLAGS   = \
 -L /usr/local/lib \
 -L $(THISDIR)/../libpredict/bin \
 -lopencv_core \
 -lopencv_highgui \
 -lboost_filesystem \
 -lboost_program_options \
 -lboost_system \
 -lpredict

endif

ifeq ($(UNAME), Linux)

INCLUDES  += \
 -I /usr/local/include/opencv2 \
 -I /usr/include 

LDFLAGS   = \
 -L /usr/lib/x86_64-linux-gnu \
 -Wl,-rpath,$(THISDIR)/../libpredict/bin \
 -lopencv_core \
 -lopencv_highgui \
 -lboost_filesystem \
 -lboost_program_options \
 -lboost_system \
 -lpredict

endif

include ../Makefile.exe

