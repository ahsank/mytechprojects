CXXFLAGS= -g -I$$HOME/local/include -I../framework -I../echotestlib -DBUILDTEST
CXX=g++
LDFLAGS=-L$$HOME/local/lib
LDLIBS=


SRCS=udpkqueueserver.cpp
OBJS=$(subst .cpp,.o,$(SRCS))

all: udpkqueueservertest

udpkqueueservertest: udpkqueueserver.o ../echotestlib/perftestmain.o
	g++ -L$$HOME/local/lib  -o udpkqueueservertest $^

depend: .depend

.depend: $(SRCS)
	rm -f ./.depend
	$(CXX) $(CXXFLAGS) -MM $^>>./.depend;

clean:
	$(RM) $(OBJS) udpkqueueservertest

dist-clean: clean
	$(RM) *~ .dependtool

include .depend

# Mac OSX
# export DYLD_LIBRARY_PATH=/Users/ahsan/local/lib
# before run


