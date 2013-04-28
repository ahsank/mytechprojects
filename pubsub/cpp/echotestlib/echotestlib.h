#pragma once
#include "framework.h"

class EchoServer: public EventHandler {
public:
    EchoServer() {
        description = "echo server";
    }
    virtual void process(char *data, int len, bool iseof) {
        INFO_OUT("Server sending response\n");
        send(data, len, iseof);
    }
};

class EchoClient: public EventHandler {
public:
    int numGot;
    int numSent;
    int maxSend;
    timeval beginTime;

    EchoClient(int nReq) :
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
