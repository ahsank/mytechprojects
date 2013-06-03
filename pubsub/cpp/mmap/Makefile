CXXFLAGS=-g -I$$HOME/local/include -I../framework -I../echotestlib -DBUILDTEST
CXX=g++
LDFLAGS=-L$$HOME/local/lib
LDLIBS=-levent


SRCS=mmapserver.cpp
OBJS=$(subst .cpp,.o,$(SRCS))

all: mmapservertest

mmapservertest: mmapserver.o ../echotestlib/perftestmain.o
	g++ -L$$HOME/local/lib  -o mmapservertest $^ 

depend: .depend

.depend: $(SRCS)
	rm -f ./.depend
	$(CXX) $(CXXFLAGS) -MM $^>>./.depend;

clean:
	$(RM) $(OBJS) mmapservertest

dist-clean: clean
	$(RM) *~ .dependtool

include .depend

# Mac OSX
# export DYLD_LIBRARY_PATH=/Users/ahsan/local/lib
# before run


