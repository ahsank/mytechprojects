CXXFLAGS=-g  -pg -I$$HOME/local/include
CXX=g++
LDFLAGS=-L$$HOME/local/lib
LDLIBS=-levent


SRCS=eventserver.cpp selectserver.cpp
OBJS=$(subst .cpp,.o,$(SRCS))

all: eventserver selectserver memcpyserver

eventserver: eventserver.o
	g++ -L$$HOME/local/lib  -o eventserver eventserver.o -levent

selectserver: selectserver.o
	g++ -g -pg -o selectserver selectserver.o 

memcpyserver: memcpyserver.o
	g++ -o memcpyserver memcpyserver.o
 
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


