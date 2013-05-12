CXXFLAGS=-g -I$$HOME/local/include -I../framework -I../echotestlib -DBUILDTEST
CXX=g++
LDFLAGS=-L$$HOME/local/lib
LDLIBS=


SRCS=udpepollserver.cpp
OBJS=$(subst .cpp,.o,$(SRCS))

all: udpepollservertest

udpepollservertest: udpepollserver.o ../echotestlib/perftestmain.o
	g++ -L$$HOME/local/lib  -o udpepollservertest $^

depend: .depend

.depend: $(SRCS)
	rm -f ./.depend
	$(CXX) $(CXXFLAGS) -MM $^>>./.depend;

clean:
	$(RM) $(OBJS) udpepollservertest

dist-clean: clean
	$(RM) *~ .dependtool

include .depend

# Mac OSX
# export DYLD_LIBRARY_PATH=/Users/ahsan/local/lib
# before run


