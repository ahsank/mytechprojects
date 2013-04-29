CXXFLAGS=-g -O3 -I$$HOME/local/include
CXX=g++
LDFLAGS=-L$$HOME/local/lib
LDLIBS=-levent

all : eventserver

SRCS=eventserver.cpp
OBJS=$(subst .cpp,.o,$(SRCS))

all: eventserver

eventserver: $(OBJS)
	g++ $(LDFLAGS) -o eventserver $(OBJS) $(LDLIBS)

depend: .depend

.depend: $(SRCS)
	rm -f ./.depend
	$(CXX) $(CPPFLAGS) -MM $^>>./.depend;

clean:
	$(RM) $(OBJS)

dist-clean: clean
	$(RM) *~ .dependtool

include .depend

# Mac OSX
# export DYLD_LIBRARY_PATH=/Users/ahsan/local/lib
# before run


