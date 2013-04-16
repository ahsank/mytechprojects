Organization
===================


* Network client
	Subscribe ('stdin', "//localhost:8000/echo/myId")
	Process event ({source, sourceid, msgid, text})
		if sourceid == 1
			publish ('//localhost:8000/echo', {myId, msgId=rand(),text})
		else 
			publish('stdout', txt}


* Network server
	Subscribe ('*:8000/echo')
	Process event {source, sourceid, msgid, text)
		publish("/echo/" + source, {myid, rand(), text + 'response'})

Regular socket
======================
* server
	bind(8000, 
		process_request(inbuffer, outbuffer)
			do
				data, len, iseof = read buffer
				write outbuffer data len
			while !iseof

*stdin echo 
	id = connect(localhost:8000, readwrite, process_response)
	id2 = open_file(stdout, write)
	open_file(stdin, read_data = 
		fn(this, buffer, iseof)
			buffer = readline()
			if !iseof
				send(id, buffer))
			else 
				close(id)
				close(id2)
				close(this)

	process_response(buffer)
		data, len, iseof = read(buffer)
		send(id2, data, len)
	
* Perf client
	id = connect('localhost:8000')
	process_echo_response = fn(Perf client p, msg, len, iseof)
		iseof ? processed++
		if (processed == 1M)
			setevent(done,true)
	time = current_time()
	done = event(false)
	for (int i=0; i < 1M; i++)
		message = get_random_message()
		send(id, message, process_echo_response)
	wait(done)
	print(current_time()-time)
		
		
Class
--------------------
FileEventHanlder fileHandler;
fileHanlder.fd = 1
fileHandler.register("fileHandler")

MyClient client 
	getEventFrom (&fileHandler, &networkHandler)
	publishTo(&networkHandler2, &stdoutHandler2)
	process event (Message *pm)
		if (pm->getSource() == sources[0])
			publisher[0]->publish(pm);
		else 
			publisher[1]->publish(pm)
MyClient servier
	getEventFrom(&networkHandler)
	publishTo(&networkHandler1)
	process event
		publisher[0]->publish(pm)
	

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
7. Write the same server in Hasell
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

Referece
-------------------------------
http://www.wangafu.net/~nickm/libevent-book/01_intro.html

