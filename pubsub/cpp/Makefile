CXXFLAGS=-g -I$$HOME/local/include
CXX=g++
LDFLAGS=-L$$HOME/local/lib
LDLIBS=-levent

all : eventserver

<<<<<<< HEAD
SRCS=eventserver.cpp
OBJS=$(subst .cpp,.o,$(SRCS))

all: eventserver

eventserver: $(OBJS)
	g++ $(LDFLAGS) -o eventserver $(OBJS) $(LDLIBS)

depend: .depend

.depend: $(SRCS)
	rm -f ./.depend
	$(CXX) $(CPPFLAGS) -MM $^>>./.depend;
=======
eventserver : eventserver.cpp
	g++ -O3 eventserver.cpp -o eventserver -levent
>>>>>>> 280ab903daacc9dd42b9301470cb2767cbecd4a6

clean:
	$(RM) $(OBJS)

dist-clean: clean
	$(RM) *~ .dependtool

include .depend

# Mac OSX
# export DYLD_LIBRARY_PATH=/Users/ahsan/local/lib
# before run


