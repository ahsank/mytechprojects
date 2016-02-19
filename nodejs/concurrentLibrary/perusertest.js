var request = require('request');
var concurrentLibrary = require("./concurrentLibrary.js");

var getTime = function () {
    return new Date().toString();
}

for (var i=0; i < 20; i++) {
    // Sends request for two different users
    concurrentLibrary.limit("user1", function () {
        request('http://localhost:3000/api/user1',
                function(error, response, body) {
                    if (!error && response.statusCode == 200) {
                        console.log(getTime() + " user1 200 okay, hurray!");
                    }
                });
    });
    concurrentLibrary.limit("user2", function () {
        request('http://localhost:3000/api/user2',
                function(error, response, body) {
                    if (!error && response.statusCode == 200) {
                        console.log(getTime() + " user2 200 okay, hurray!");
                    }
                });
    });

}



