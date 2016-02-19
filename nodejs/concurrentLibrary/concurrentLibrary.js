// Time tracker class
function TimeTracker() {
    var timeMs = (new Date).getTime();
    // Array of time when api is called, in increasing order
    this.times = [timeMs];
    this.intervalMs = 1000; // 1 second
    this.maxPending = 10;
}

// removes all elements < current time - 1 seconds
TimeTracker.prototype.cleanup = function(timeMs) {
    var count = 0;
    for (var i=0; i < this.times.length; i++) {
        if (this.times[i] > timeMs - this.intervalMs)
            break;
        count++;
    }
    if (count > 0) {
        this.times.splice(0, count);
    }
}

// Returns minimum time to wait to guarantee that
// not more than maxPending call is made.
TimeTracker.prototype.getWaitTime = function () {
    var timeMs = (new Date).getTime();
    this.cleanup(timeMs);
    if (this.times.length < this.maxPending) {
        this.times.push(timeMs);
        return 0;
    }    
    return this.times[0] + 1000 - timeMs;
}

var users = {};

function checkLimit(tracker, callback) {
    var waitTime = tracker.getWaitTime();
    if (waitTime == 0) {
        callback();
        return;
    }
    setTimeout(checkLimit, waitTime, tracker, callback);
}

var exports = module.exports = {};
exports.limit = function(userid, callback) {
        if (!(userid in users)) {
            users[userid] = new TimeTracker();
            callback();
            return;
        }
        tracker = users[userid];
        checkLimit(tracker, callback);
}




