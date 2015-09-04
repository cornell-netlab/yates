THISDIR := $(realpath .)

EXECUTABLE = predict

CPPFLAGS  =
INCLUDES  =
LDFLAGS   =

OBJDIR  = $(THISDIR)
BINDIR  = $(THISDIR)
SRCDIR  = $(THISDIR)
INCDIR  = $(THISDIR)

## Implicit Section: change the following only when necessary.
CXX      = g++
CXXFLAGS = -g -O2 -fPIC -Wall 
SRCDIRS  = $(shell find $(SRCDIR) -type d)
INCDIRS  = $(shell find $(INCDIR) -type d)
INCFLAGS = $(addprefix -I ,$(SRCDIRS)) $(addprefix -I ,$(INCDIRS)) $(INCLUDES)

## Stable Section: usually no need to be changed. 
SHELL   = /bin/sh
SOURCES = $(wildcard $(addsuffix /*.cpp,$(SRCDIRS)))
OBJS    = $(patsubst $(SRCDIR)/%,$(OBJDIR)/%,$(SOURCES:.cpp=.o))
DEPS    = $(OBJS:.o=.d)

## Define some useful variables.
CDP = g++
DEP_OPT = -MM -MP
DEPEND  = $(CDP) $(CPPFLAGS) $(CXXFLAGS) $(INCFLAGS) $(DEP_OPT)
COMPILE = $(CXX) $(CPPFLAGS) $(CXXFLAGS) $(INCFLAGS)
LINK    = $(CXX) $(CPPFLAGS) $(CXXFLAGS) $(LDFLAGS)

.PHONY: all objs clean distclean help show

# Delete the default suffixes
.SUFFIXES:

$(OBJDIR)/%.d: $(SRCDIR)/%.cpp
	#@mkdir -p $(dir $@)
	@/bin/echo -n $(dir $@) > $@
	@$(DEPEND) $< >> $@ || rm $@

all:

objs: $(OBJS)

$(OBJDIR)/%.o: $(SRCDIR)/%.cpp
	@echo "Compiling $@"
	@$(COMPILE) -c $< -o $@

ifneq ($(DEPS),)
  sinclude $(DEPS)
endif

clean:
	@$(RM) -r $(OBJS) $(EXECUTABLE) *.d
	@$(RM) -r docs/html

all: $(BINDIR)/$(EXECUTABLE)

$(BINDIR)/$(EXECUTABLE): $(OBJS)
	@echo "Building $@"
	#@mkdir -p $(dir $@)
	@$(LINK) $(OBJS) -o "$@"


