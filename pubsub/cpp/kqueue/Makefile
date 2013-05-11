CXXFLAGS=-g -I$$HOME/local/include -I../framework -I../echotestlib -DBUILDTEST
CXX=g++
LDFLAGS=-L$$HOME/local/lib
LDLIBS=-levent


SRCS=kqueueserver.cpp
OBJS=$(subst .cpp,.o,$(SRCS))

all: kqueueservertest

kqueueservertest: kqueueserver.o ../echotestlib/perftestmain.o
	g++ -L$$HOME/local/lib  -o kqueueservertest $^

depend: .depend

.depend: $(SRCS)
	rm -f ./.depend
	$(CXX) $(CXXFLAGS) -MM $^>>./.depend;

clean:
	$(RM) $(OBJS) kqueueservertest

dist-clean: clean
	$(RM) *~ .dependtool

include .depend

# Mac OSX
# export DYLD_LIBRARY_PATH=/Users/ahsan/local/lib
# before run


