#include <errno.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <iostream>
#include <sys/time.h>
#include <arpa/inet.h>
#include <netinet/in.h>
#include <sys/socket.h>
#include <fcntl.h>
#include <sys/select.h>
#include <assert.h>

//
// Server and client in the same process communicating though copying
// data using memcpy. It is to test overhead of class infrastructure. On
// Intel i7 Mac OSX, there were 11M roundtrip messages/sec
//


unsigned int traceLevel = 0;

// Behaves similarly to printf(...), but adds file, line, and function
// information. I omit do ... while(0) because I always use curly braces in my
// if statements.
#define INFO_OUT(...) if (traceLevel){\
printf("%s:%d: %s():\t", __FILE__, __LINE__, __FUNCTION__);\
printf(__VA_ARGS__);\
putc('\n', stdout); \
fflush(stdout); \
}

// Behaves similarly to fprintf(stderr, ...), but adds file, line, and function
// information.
#define ERROR_OUT(...) {\
fprintf(stderr, "\033[0;1m%s:%d: %s():\t", __FILE__, __LINE__, __FUNCTION__);\
fprintf(stderr, __VA_ARGS__);\
fprintf(stderr, "\e[0m");\
putc('\n', stderr); \
fflush(stderr); \
}

// Behaves similarly to perror(...), but supports printf formatting and prints
// file, line, and function information.
#define ERRNO_OUT(...) {\
fprintf(stderr, "\033[0;1m%s:%d: %s():\t", __FILE__, __LINE__, __FUNCTION__);\
fprintf(stderr, __VA_ARGS__);\
fprintf(stderr, ": %d (%s)\e[0m\n", errno, strerror(errno));\
putc('\n', stderr); \
fflush(stderr);\
}

long int getTimeDiff(struct timeval *t2, struct timeval *t1)
{
    return (t2->tv_usec + 1000000 * t2->tv_sec) - (t1->tv_usec + 1000000 * t1->tv_sec);
}

void printCurrentTime()
{
    char buffer[30];
    time_t curtime;
    timeval currTime;

    gettimeofday(&currTime, NULL);
    curtime = currTime.tv_sec;
    strftime(buffer, 30, "%m-%d-%Y  %T", localtime(&curtime));
    printf("%s.%06ld\n", buffer, (long)currTime.tv_usec);
}


class Processor {

public:

	Processor() :
    parent(NULL) {
	}

	virtual void initialize() {
	}

	virtual void process() {
	}

	virtual void enable() {
	}

	virtual ~Processor() {
	}

protected:

	Processor *parent;
	friend class SelectMain;

};



struct Context;

class SelectMain;

class SelectHandler: public Processor {
protected:
	char *m_output;
	size_t m_outputLength;
	bool isMore;
	Context *pContext;
	const char *description;

public:
	SelectHandler() :
    m_output(NULL), m_outputLength(0), isMore(false), pContext(NULL),
    description(
                "generic") {
	}

	const char *getDescription() {
		return description;
	}

	void setContext(Context *pContext) {
		this->pContext = pContext;
	}

	Context * getContext() {
		return pContext;
	}

	void send(char *data, int len, bool iseof);

	virtual void process(char *data, int length, bool iseof) = 0;

	SelectMain *getParent() {
		return (SelectMain*) this->parent;
	}

};

const int max_buff = 32767;

struct ClientState {
    SelectHandler *handler;
};

class SelectMain: public Processor {
protected:

    char *buffer;
    int len;
    SelectHandler *server;
    SelectHandler *client;
    bool loopEnd;
    SelectHandler *dest;


public:

	void initialize() {
        loopEnd = false;
        buffer = new char[max_buff];
        len = 0;
        server = client = dest = NULL;

	}
	void process() {

		while (!loopEnd) {
            if (len == 0) {
                continue;
            }
            if (dest) {
                SelectHandler *context = NULL;
                context = (dest == client) ? server : client;
                dest->setContext((Context*)(void*)context);
                int tmplen = len;
                len = 0;
                dest->process(buffer, tmplen, true);
            }
        }

    }


    void cancelLoop() {
        loopEnd = true;
    }

    void bindServer(const char *port, SelectHandler *pProcessor) {
        this->server = pProcessor;
        pProcessor->parent = this;
     }

    void send(SelectHandler *p, char *data, int len, bool isDataEnd) {
        if (!p) {
            INFO_OUT("Invalid context");
            return;
        }
        memcpy(this->buffer, data, len);
        this->dest = (SelectHandler *)p->getContext();
        this->len = len;

    }

    void connectToServer(const char *address, const char *port,
                         SelectHandler *pProcessor) {
        this->client = pProcessor;
        pProcessor->parent = this;
        pProcessor->setContext((Context*)(void*)server);
        pProcessor->enable();

        // Investigate: should set reuse address ?


    }

};


void SelectHandler::send(char *data, int len, bool iseof) {
    ((SelectMain*) this->parent)->send(this, data, len, iseof);
}

class LibEventEchoServer: public SelectHandler {
public:
    LibEventEchoServer() {
        description = "echo server";
    }
    virtual void process(char *data, int len, bool iseof) {
        INFO_OUT("Server sending response");
        send(data, len, iseof);
    }
};

class LibEventEchoClient: public SelectHandler {
public:
    int numGot;
    int numSent;
    int maxSend;
    timeval beginTime;

    LibEventEchoClient(int nReq) :
    maxSend(nReq) {
        numSent = numGot = 0;
        description = "echo client";
    }

    virtual void process(char *data, int len, bool iseof) {
        numGot++;
        INFO_OUT("Client process response %d", numGot);
        if (numGot == 1) {
            printCurrentTime();
            gettimeofday(&beginTime, NULL);
        }
        if (numGot == maxSend) {
            timeval endTime;
            gettimeofday(&endTime, NULL);
            printCurrentTime();
            unsigned long timediff = getTimeDiff(&endTime, &beginTime);
            printf("Number of message %d, usec %ld, Number of message per sec %ld\n", maxSend,
                   timediff, maxSend*1000000UL / (timediff ? timediff : 1));

            getParent()->cancelLoop();
            return;
        }
        static char buffer[] = "Hello world!";
        INFO_OUT("Sending data %d", numSent);
        send(buffer, sizeof(buffer), true);
        numSent++;
    }

    virtual void enable() {
        INFO_OUT("Echo client enabled");
        static char buffer[] = "Hello world!";
        INFO_OUT("Sending data %d", numSent);
        send(buffer, sizeof(buffer), true);
        numSent++;

    }
};


bool isClientOnly;
bool isServerOnly;
const char *pAddress = "127.0.0.1";
const char *pPort = "8000";
const char *opt = "csp:a:";

void parseArgs(int argc, char **argv) {
    int c;
    while ((c = getopt(argc, argv, opt)) != -1) {
        switch (c) {
            case 'c': isClientOnly = true; break;
            case 's': isServerOnly = true; break;
            case 'p': pPort = optarg; break;
            case 'a': pAddress = optarg; break;
            default:
                fprintf(stderr, "./eventserver [-cs] [-p port] [-a address]\n");
                exit(1);

        }
    }
}

int main(int argc, char **argv) {
    parseArgs(argc, argv);
    SelectMain mainProcessor;
    LibEventEchoServer server;
    LibEventEchoClient client(1000);
    mainProcessor.initialize();
    server.initialize();
    client.initialize();
    if (!isClientOnly) {
        mainProcessor.bindServer(pPort, &server);
    }
    if (!isServerOnly) {
        mainProcessor.connectToServer(pAddress, pPort, &client);
    }
    mainProcessor.process();

}

