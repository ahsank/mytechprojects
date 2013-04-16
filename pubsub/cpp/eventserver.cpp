#include <errno.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <event2/event.h>
#include <event2/buffer.h>
#include <event2/bufferevent.h>

#define assume(cond , err) if (!cond) {perror(err);}

class Environment {
public:
	void logError(char *error);
protected:

};

class Processor {

public:

	Processor() : parent(NULL) {}

	virtual void initialize() {}

	virtual void process() {}

	Environment &env() {
		return this->environment;
	}
	virtual ~Processor() {}

protected:

	Environment environment;
	Processor *parent;

};

struct ByteOut {
	char *pData;
	size_t len;
	bool isMore;
};

class LibEventServer : public Processor {
protected:
	char *m_output;
	size_t m_outputLength;
	bool isMore;

public:
	LibEventServer() : m_output(NULL), m_outputLength(0), isMore(false) {}

	ByteOut getOutput() {
		ByteOut output = {m_output, m_outputLength, isMore};
		return output;
	}

	virtual void process(char *data, int length);

};

class EventDispatch {
	LibEventServer *m_p
	EventDispatch(LibEventServer *p) : m_p(p) {}
	virtual void dispatch (char *data, int len, bool isComplete);
	virtual void ~EventDispatch() {}
};


class LibEventMain: public Processor {
protected:
	const int max_buff = 32767;

	event_base *m_ebase;


public:

	void initialize() {
		if ((m_ebase = event_base_new()) != NULL) {
			perror("Cannot initialize libevent");
		}

	}
	void process() {
		event_base_dispatch (ebase);
	}


	static void acceptfn(int socket, short event, void *arg);
	static void readfn(bufferevent *bev, void *arg);
	static void errorfn(bufferevent *bev, short error, void *arg);

	void bindServer(int port, LibEventServer *pProcessor) {
		sockaddr_in sin;

		sin.sin_family = AF_INET;
		sin.sin_addr = 0;
		sin.sin_port = htons(port);

		int listener = socket(AF_INET, SOCK_STREAM, 0);
		evutil_make_socket_nonblocking(listener);

		if (bind(listener, (sockaddr*) &sin, sizeof(sin)) < 0) {
			perror("bind");
		}

		if (listen(listener, 16) < 0) {
			perror("listen");
			return;
		}

		pProcessor->parent = this;

		event *e = event_new(m_ebase, listener, EV_READ | EV_PERSIST, acceptfn,
				(void*) pProcessor);

		event_add(e, NULL);

	}
	void connectClient(unsigned long address, int port, LibEventServer *pProcessor) {

	}

};

void LibEventMain::readfn(bufferevent *bev, void *arg) {
	LibEventServer *p = (LibEventServer *)arg;
	evbuffer *input, *output;

	size_t n;
	int i;

	input = bufferevent_get_input(bev);
	output = bufferevent_get_output(bev);

//	size_t buffer_len = evbuffer_get_length(input);
//
	char buffer[max_buff];
//
//	if (buffer_len > max_buff) buffer_len = max_buff;
//	buffer_len = evbuffer_copyout(input, buffer, buffer_len);
//
//	p->process(buffer, buffer_len);
//
	while ((n = evbuffer_remove(input, buffer, sizeof(buffer)))>0) {
		p->process(buffer, n);
		ByteOut out = p->getOutput();
		if (out.len > 0) {
			evbuffer_add(output, out.pData, out.len);
		}
	}

	// evbuffer_drain(input, buffer_len);
	// Todo: What happens to the remaining buffer that is less than max_buff
}

void LibEventMain::errorfn(bufferevent *bev, short int error, void *arg) {
	// if error & BEV_EVENT_EOF, BEV_EVENT_ERROR, BEV_EVENT_TIMEOUT
	bufferevent_free(bev);
}

void LibEventMain::acceptfn(int listener, short event, void *arg) {
	Processor *processor = (Processor*) arg;
	LibEventMain *plevent = (LibEventMain*) processor->parent;

	sockaddr_storage ss;
	socklen_t slen = sizeof(ss);
	int fd = accept(listener, (sockaddr*) &ss, &slen);
	if (fd < 0) {
		perror("accept");
		return;
	}
	bufferevent *bev;
	evutil_make_socket_nonblocking(fd);
	bev = bufferevent_socket_new(plevent->m_ebase, fd, BEV_OPT_CLOSE_ON_FREE);
	bufferevent_setcb(bev, readfn, NULL, errorfn, arg);
	bufferevent_setwatermark(bev, EV_READ, 0, max_buff);
	bufferevent_enable(bev, EV_READ | EV_WRITE);
}

class LibEventEchoServer: public LibEventServer {
public:

	virtual void process(char *data, int len, bool iseof) {
		parent->sendResponse(this, data, len, iseof);
	}
};

class LibEventEchoClient : public LibEventServer {
public:
	int numInput;
	int requiredInput;


	virtual void process(char *data, int len, bool iseof) {
		if (iseof) {
			numInput++;
		}
		if (numInput == requiredInput) {
			parent->send(cancelEvent, NULL, 0, true);
		}
	}
};

class LibEventCancel : public LibEventServer {
	virtual void process(char *data, int len, bool iseof) {
		if (iseof) {
			parent->cancelLoop();
		}
	}
};

int main(int argc, char **argv) {

	LibEventMain mainProcessor;
	LibEventEchoServer server;
	LibEventCancel cancelClient;
	LibEventEchoClient client;
	mainProcessor.initialize();
	server.initialize();
	mainProcessor.bindServer(8000, &server);
	mainProcessor.process();
	char buffer[1024];
	for (int i=0; i < 1000; i++) {
		client.send(buffer, 1024, true);
	}

}

