console.log("Hello World!");

concurrent = require("./concurrentLibrary.js");

var getTime = function () {
    return new Date().toString();
}
for (var i=0; i < 20; i++) {
    concurrent.limit("10", function () {
        console.log("Called at " + getTime())
    });
}
