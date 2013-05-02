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

	int listener;
    int dest;
    SelectHandler *server;
    SelectHandler *client;
    bool loopEnd;
	ClientState *pStates;

public:

	void initialize() {
        loopEnd = false;
        dest = listener = -1;

	}
    void buildfds(ClientState **
                  states, int *fds, int *pnumfds) {
        int numfds = 0;
        for(int i=0; i < FD_SETSIZE; i++) {
            if (!states[i]) continue;
            fds[numfds++] = i;
        }
        *pnumfds = numfds;
        
    }
    
	void process() {
        int fds[FD_SETSIZE];
        int numfds =0;
        struct ClientState *states[FD_SETSIZE];
		int maxfd;
        fd_set readset, writeset, exset;
		for (int i = 0; i < FD_SETSIZE; ++i)
            states[i] = NULL;

        FD_ZERO(&readset);
        FD_ZERO(&writeset);
        FD_ZERO(&exset);
        if (dest != -1) {
            struct ClientState *state = (ClientState*)malloc(sizeof(struct ClientState));
            assert(state);
            state->handler = this->client;
            states[dest] = state;
        }
        if (listener != -1) {
            FD_SET(listener, &readset);
        }
        maxfd = listener;
        for(int i=0; i < FD_SETSIZE; i++) {
            if (!states[i]) continue;
            if (i > maxfd) {
                maxfd = i;
            }
            FD_SET(i, &readset);
        }
        buildfds(states, fds, &numfds);
        
        INFO_OUT("Listening socket %d", listener);
        INFO_OUT("Connected socket %d", dest);

		while (!loopEnd) {

            FD_ZERO(&readset);
            FD_ZERO(&writeset);
            FD_ZERO(&exset);

            if (listener != -1) {
                FD_SET(listener, &readset);
            }
            maxfd = listener;
            for(int i=0; i < numfds; i++) {
                if (fds[i] > maxfd) {
                    maxfd = fds[i];
                }
                FD_SET(fds[i], &readset);
            }
            INFO_OUT("slecting %d sockets", numfds);
            int numResult;
			if ((numResult = select(maxfd+1, &readset, NULL, NULL, NULL)) < 0) {
                perror("select");
                return;
            }
            INFO_OUT("selected %d sockets", numResult);
            if (listener != -1 && FD_ISSET(listener, &readset)) {
                struct sockaddr_storage ss;
                socklen_t slen = sizeof(ss);
                int fd = accept(listener, (struct sockaddr*)&ss, &slen);
                if (fd < 0) {
                    perror("accept");
                } else if (fd > FD_SETSIZE) {
                    close(fd);
                } else {
                    fcntl(fd, F_SETFL, O_NONBLOCK);
                    struct ClientState *state = (ClientState*)malloc(sizeof(struct ClientState));
                    assert(state);
                    state->handler = server;
                    states[fd] = state;
                    fds[numfds++] = fd;
                    INFO_OUT("Accepted socket %d", fd);

                }

        	}

        	for (int i=0; i < maxfd+1; ++i) {
                int r = 0;
                if (i == listener)
                    continue;

                if (FD_ISSET(i, &readset)) {
                    char buf[1024];
                    ssize_t result;

                    while(1) {
                        INFO_OUT("Reading socket %d", i);
                        result = recv(i, buf, sizeof(buf), 0);
                        if (result < 0) {
                            perror("recv");
                            break;
                        }
                        else if (result == 0) {
                            break;
                        }
                        if (states[i] && states[i]->handler) {
                            states[i]->handler->setContext((Context*)(long)i);
                            states[i]->handler->process(buf, result, true);
                        }
                        break;

                    }
                    r = (result == 0) || (result < 0 && result != EAGAIN);

                }
                //if (r == 0 && FD_ISSET(i, &writeset)) {
                //r = do_write(i, state[i]);
                //}
                if (r) {
                    INFO_OUT("Closing socket %d", i);
                    free(states[i]);
                    states[i] = NULL;
                    close(i);
                    buildfds(states, fds, &numfds);
            	}
            }

        }
    }


    void cancelLoop() {
        loopEnd = true;
    }

    void bindServer(const char *port, SelectHandler *pProcessor) {
        struct sockaddr_in sin = {0};

        sin.sin_family = AF_INET;
        sin.sin_addr.s_addr = 0;
        sin.sin_port = htons(atoi(port));

        listener = socket(AF_INET, SOCK_STREAM, 0);
        this->server = pProcessor;
        pProcessor->parent = this;
        fcntl(listener, F_SETFL, O_NONBLOCK);
        int oneval = 1;
        setsockopt(listener, SOL_SOCKET, SO_REUSEADDR, &oneval, sizeof(oneval));
        if (bind(listener, (struct sockaddr*)&sin, sizeof(sin)) < 0) {
            perror("bind");
            return;
        }
        INFO_OUT("Bound to port %s", port);
        if (listen(listener, 16)<0) {
            perror("listen");
            return;
        }
        INFO_OUT("Listenning to port %s", port);
    }

    void send(SelectHandler *p, char *data, int len, bool isDataEnd) {
        if (!p) {
            INFO_OUT("Invalid context");
            return;
        }
        if (::send((int)(long)p->getContext(), data, len, 0) == -1) {
            perror("send");
        }
        INFO_OUT("Done sending");

    }

    void connectToServer(const char *address, const char *port,
                         SelectHandler *pProcessor) {
        sockaddr_in sin = { 0 };

        sin.sin_family = AF_INET;
        sin.sin_port = htons(atoi(port));
        inet_pton(AF_INET, address, &(sin.sin_addr));
        dest = socket(AF_INET, SOCK_STREAM, 0);
        this->client = pProcessor;
        if (connect(dest, (sockaddr*)&sin, sizeof(sin)) < 0) {
            perror("connect");
            exit(1);
            return;
        }
        pProcessor->parent = this;
        pProcessor->setContext((Context*)(long)dest);
        fcntl(dest, F_SETFL, O_NONBLOCK);
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

