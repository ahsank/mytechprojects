

Works
-----------------------------------
1. Socket server
	a. Single threaded select based
	b. Processing loop
	c. Disconnection handling
	d. 

2. Future server
	a. Buffer management, zero copy?
	b. Multithreaded io handling
	
Plan
------------------------------------
1. Write a C kpoll server
2. Write a Echo server
3. Write same server using eventlib
4. Write the same server using zeromq
5. Perf test
6. Write the same server im java
7. Write the same server in Ocaml
8. Write the same server in Erlang
9. Class design for generic server and client
10. Pub/sub feature design
	a. Reliable
	b. multicast
	c. flow control
	d. Last value
	e. Transactional
	f. Large number of nodes
	g. POint to point
	h. Broker based
	i. Topic based subscribing
	j. Authn/Authz
	k. Object based routing
	l. 

Todo
--------------------------------
1. Write a unit test for the socket
2.

Referece
-------------------------------
EventLib:
http://www.wangafu.net/~nickm/libevent-book/01_intro.html
https://github.com/nitrogenlogic/cliserver/blob/master/cliserver.c

