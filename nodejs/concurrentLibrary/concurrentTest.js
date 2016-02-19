var request = require('request');
var concurrentLibrary = require("./concurrentLibrary.js");

var getTime = function () {
    return new Date().toString();
}

for (var i=0; i < 32; i++) {
    concurrentLibrary.limit("user1", function () {
        request('http://localhost:3000/api',
                function(error, response, body) {
                    if (!error && response.statusCode == 200) {
                        console.log("200 okay, hurray!");
                    }
                });
    });
}

for (var i=0; i < 5; i++) {
    concurrentLibrary.limit("user1", function () {
        request('http://localhost:3000/api',
                function(error, response, body) {
                    if (!error && response.statusCode == 200) {
                        console.log("200 okay, hurray!");
                    }
                });
    });
}

