Result
--------------------------------------

- Client sends a message to server and sends next message after getting response

Measured in Intel core i7

<pre>

Method          | OS            | Process       | Messages /sec
livevent        | OS X          | Same          | 29K
                |               | Different     | 28K 
                | Linux VM      | Same          | 54 K
                |               | Different     | 12K
Select          | OS X          | Same          | 39K
                |               | Different     | 41K
                | Linux VM      | Same          | 68K
                |               | Different     | 15K
memcpy          | OS X          | Same          | 11M

</pre>

Plan
------------------------------------
1. Write a C kpoll server
2. Write a Echo server
3. Write same server using eventlib

- [x] Single threaded server.
- [x] Single threaded client.
- [ ] Test client disconnect handling
- [ ] Unit tests
- [ ] Check whether memory leak
- [ ] Test performance on non virtual machine
	
4. Write the same server using zeromq
5. Perf test
6. Write the same server im java
7. Write the same server in Ocaml
8. Write the same server in Erlang
9. Class design for generic server and client
10. Pub/sub feature design
	1. Reliable
	2. multicast
	3. flow control
	4. Last value
	5. Transactional
	6. Large number of nodes
	7. POint to point
	8. Broker based
	9. Topic based subscribing
	10. Authn/Authz
	11. Object based routing
	l2. 
11. Write using blocking socket and lots of threads.


Future
--------------------------------
1. Write a unit test for the socket
1. Buffer management, zero copy?
2. Multithreaded io handling

Referece
-------------------------------
*EventLib*

http://www.wangafu.net/~nickm/libevent-book/01_intro.html

https://github.com/nitrogenlogic/cliserver/blob/master/cliserver.c
https://idea.popcount.org/2012-09-11-concurrent-queue-in-c/


