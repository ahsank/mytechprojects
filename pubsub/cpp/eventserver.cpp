#include <errno.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <event2/event.h>
#include <event2/buffer.h>
#include <event2/bufferevent.h>
#include <iostream>
#include <sys/time.h>
#include <arpa/inet.h>


unsigned int traceLevel = 0;

// Behaves similarly to printf(...), but adds file, line, and function
// information. I omit do ... while(0) because I always use curly braces in my
// if statements.
#define INFO_OUT(...) if (traceLevel){\
printf("%s:%d: %s():\t", __FILE__, __LINE__, __FUNCTION__);\
printf(__VA_ARGS__);\
	fflush(stdout); \
}

// Behaves similarly to fprintf(stderr, ...), but adds file, line, and function
// information.
#define ERROR_OUT(...) {\
fprintf(stderr, "\033[0;1m%s:%d: %s():\t", __FILE__, __LINE__, __FUNCTION__);\
fprintf(stderr, __VA_ARGS__);\
fprintf(stderr, "\e[0m");\
	fflush(stderr); \
}

// Behaves similarly to perror(...), but supports printf formatting and prints
// file, line, and function information.
#define ERRNO_OUT(...) {\
fprintf(stderr, "\033[0;1m%s:%d: %s():\t", __FILE__, __LINE__, __FUNCTION__);\
fprintf(stderr, __VA_ARGS__);\
fprintf(stderr, ": %d (%s)\e[0m\n", errno, strerror(errno));\
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
	friend class LibEventMain;

};



struct Context;

class LibEventMain;

class LibEventHandler: public Processor {
protected:
	char *m_output;
	size_t m_outputLength;
	bool isMore;
	Context *pContext;
	const char *description;

public:
	LibEventHandler() :
			m_output(NULL), m_outputLength(0), isMore(false), pContext(NULL), description(
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

	LibEventMain *getParent() {
		return (LibEventMain*) this->parent;
	}

};

const int max_buff = 32767;

class LibEventMain: public Processor {
protected:
	event_base *m_ebase;

public:

	void initialize() {
		if ((m_ebase = event_base_new()) == NULL) {
			perror("Cannot initialize libevent");
		}

	}
	void process() {
		event_base_dispatch(m_ebase);
	}

	void cancelLoop() {
		if (event_base_loopexit(m_ebase, NULL)) {
			ERROR_OUT("Error shutting down the server\n");
		}
	}

	static void acceptfn(int socket, short event, void *arg);
	static void readfn(bufferevent *bev, void *arg);
	static void errorfn(bufferevent *bev, short error, void *arg);

	void bindServer(const char *port, LibEventHandler *pProcessor) {
		sockaddr_in sin = { 0 };

		sin.sin_family = AF_INET;
		sin.sin_port = htons(atoi(port));

		int listenerfd = socket(AF_INET, SOCK_STREAM, 0);
		if (listenerfd == -1) {
			ERRNO_OUT("Error creating listening socket");
			exit(1);
		}
		int reuse = 1;
		if (setsockopt(listenerfd, SOL_SOCKET, SO_REUSEADDR, &reuse,
				sizeof(reuse))) {
			ERRNO_OUT("Error enabling socket reuse");
			exit(1);
		}
		evutil_make_socket_nonblocking(listenerfd);

		if (bind(listenerfd, (sockaddr*) &sin, sizeof(sin)) < 0) {
			perror("bind");
		}

		if (listen(listenerfd, 16) < 0) {
			perror("listen");
			return;
		}

		pProcessor->parent = this;

		event *e = event_new(m_ebase, listenerfd, EV_READ | EV_PERSIST,
				acceptfn, (void*) pProcessor);

		event_add(e, NULL);

		INFO_OUT("Bound to port:%s\n", port);

	}

	void send(LibEventHandler *p, char *data, int len, bool isDataEnd) {
		if (!data || !len) {
			return;
		}
		bufferevent *bev = (bufferevent *) p->getContext();
		if (!bev) {
			return;
		}
		bufferevent_write(bev, data, len);
		//bufferevent_flush(bev, EV_WRITE, BEV_NORMAL);

	}

	void connectToServer(const char *address, const char *port,
			LibEventHandler *pProcessor) {
		sockaddr_in sin = { 0 };

		sin.sin_family = AF_INET;
		sin.sin_port = htons(atoi(port));
		inet_pton(AF_INET, address, &(sin.sin_addr));

		// Investigate: set reuse address and make socket nonblocking?

		bufferevent *bev = bufferevent_socket_new(m_ebase, -1,
				BEV_OPT_CLOSE_ON_FREE);

		bufferevent_setcb(bev, readfn, NULL, errorfn, (void*) pProcessor);
		pProcessor->setContext((Context*) bev);
		pProcessor->parent = this;
		if (bufferevent_socket_connect(bev, (struct sockaddr *) &sin,
				sizeof(sin)) < 0) {
			ERROR_OUT("Cannot bind to port %s", port);
			/* Error starting connection */
			bufferevent_free(bev);
			exit(1);
		}

	}

};

void LibEventMain::readfn(bufferevent *bev, void *arg) {
	LibEventHandler *p = (LibEventHandler *) arg;
	INFO_OUT("Readfn %s:\n", (p ? p->getDescription() : "None") );
	evbuffer *input, *output;

	size_t n;
	int i;

	input = bufferevent_get_input(bev);

	char buffer[max_buff];

//
//	char *data = evbuffer_readln(input, &n, EVBUFFER_EOL_LF);
//	p->process(data, n, true);
//	free(data);
	if ((n = evbuffer_remove(input, buffer, sizeof(buffer))) > 0) {
		p->process(buffer, n, !n);
	}

	// Todo: What happens to the remaining buffer that is less than max_buff
}

void LibEventMain::errorfn(bufferevent *bev, short int error, void *arg) {
	INFO_OUT("Errorfn: %x\n", error);
	if (error & BEV_EVENT_CONNECTED) {
		bufferevent_setwatermark(bev, EV_READ, 0, max_buff);
		bufferevent_enable(bev, EV_READ | EV_WRITE);
		LibEventHandler *p = (LibEventHandler *) arg;
		if (p) {
			p->enable();
		}
	}
	// if error & BEV_EVENT_EOF, BEV_EVENT_ERROR, BEV_EVENT_TIMEOUT
	if ((error & BEV_EVENT_ERROR) || (error & BEV_EVENT_EOF)
			|| (error & BEV_EVENT_TIMEOUT)) {
		bufferevent_free(bev);
	}
}

void LibEventMain::acceptfn(int listener, short event, void *arg) {
	LibEventHandler *processor = (LibEventHandler*) arg;
	LibEventMain *plevent = (LibEventMain*) processor->parent;

	sockaddr_storage ss;
	socklen_t slen = sizeof(ss);
	// Investigate: Using a loop to accept connections makes it faster?
	int fd = accept(listener, (sockaddr*) &ss, &slen);
	if (fd < 0) {
		// Investigate: Should check EWOULDBLOCK and EAGAIN?
		perror("accept");
		return;
	}
	INFO_OUT("Client connected on fd %d\n", fd);

	bufferevent *bev;
	evutil_make_socket_nonblocking(fd);
	bev = bufferevent_socket_new(plevent->m_ebase, fd, BEV_OPT_CLOSE_ON_FREE);
	processor->setContext((Context*) bev);
	bufferevent_setcb(bev, readfn, NULL, errorfn, arg);
	bufferevent_setwatermark(bev, EV_READ, 0, max_buff);
	bufferevent_enable(bev, EV_READ | EV_WRITE);

}

void LibEventHandler::send(char *data, int len, bool iseof) {
	((LibEventMain*) this->parent)->send(this, data, len, iseof);
}

class LibEventEchoServer: public LibEventHandler {
public:
	LibEventEchoServer() {
		description = "echo server";
	}
	virtual void process(char *data, int len, bool iseof) {
		INFO_OUT("Server sending response\n");
		send(data, len, iseof);
	}
};

class LibEventEchoClient: public LibEventHandler {
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
		INFO_OUT("Client process response %d\n", numGot);
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
		static char buffer[] = "Hello world!\n";
		INFO_OUT("Sending data %d\n", numSent);
		send(buffer, sizeof(buffer), true);
		numSent++;
	}

	virtual void enable() {
		INFO_OUT("Echo client enabled\n");
		static char buffer[] = "Hello world!\n";
		INFO_OUT("Sending data %d\n", numSent);
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
	LibEventMain mainProcessor;
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

