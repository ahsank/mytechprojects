CXXFLAGS=-g -I$$HOME/local/include -I../framework -I../echotestlib -DBUILDTEST
CXX=g++
LDFLAGS=-L$$HOME/local/lib
LDLIBS=-levent


SRCS=epollserver.cpp
OBJS=$(subst .cpp,.o,$(SRCS))

all: epollservertest

epollservertest: epollserver.o ../echotestlib/perftestmain.o
	g++ -L$$HOME/local/lib  -o epollservertest $^

depend: .depend

.depend: $(SRCS)
	rm -f ./.depend
	$(CXX) $(CXXFLAGS) -MM $^>>./.depend;

clean:
	$(RM) $(OBJS) epollservertest

dist-clean: clean
	$(RM) *~ .dependtool

include .depend

# Mac OSX
# export DYLD_LIBRARY_PATH=/Users/ahsan/local/lib
# before run


