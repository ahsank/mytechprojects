# Concurrent RequestL ibrary

The goal of the concurrent request library is to limit number of requests
sent per user to 10 per seconds.

## Design

1. It keeps an array of last request sent timestamp per user.
2. Before sending each request or a user.
  a. Remove from the array all timestamps earlier than current time minus 1 seconds.
  b. If the array length is less than 10
     1. Add current timestamp in the array.
     2. Send the request
  c. Else wait for (earliest time stored in the array + 1 seconds - current time)

All the checking is done synchronously without any locking as node js is single threaded.

### Testing calling a webservice

Start the REST server:

    cd ../testapi
    npm start


Run the tests:


    cd ../concurrentLibrary
    node concurrentTest.js
    node perusertest.js

