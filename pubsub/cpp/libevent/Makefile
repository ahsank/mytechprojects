CXXFLAGS=-g -I$$HOME/local/include -I../framework -I../echotestlib
CXX=g++
LDFLAGS=-L$$HOME/local/lib
LDLIBS=-levent


SRCS=eventserver.cpp
OBJS=$(subst .cpp,.o,$(SRCS))

all: eventserver 

eventserver: eventserver.o
	g++ -L$$HOME/local/lib  -o eventserver eventserver.o -levent

depend: .depend

.depend: $(SRCS)
	rm -f ./.depend
	$(CXX) $(CXXFLAGS) -MM $^>>./.depend;

clean:
	$(RM) $(OBJS)

dist-clean: clean
	$(RM) *~ .dependtool

include .depend

# Mac OSX
# export DYLD_LIBRARY_PATH=/Users/ahsan/local/lib
# before run


