var express = require('express');
var router = express.Router();

function getFullDigits(number) {
    return (number < 10 ? "0" : "") + number;
}
function getTime() {

    var date = new Date();

    var hour = getFullDigits(date.getHours());
    var min  = getFullDigits(date.getMinutes())
    var sec  = getFullDigits(date.getSeconds());
    return hour + ":" + min + ":" + sec;
}

/* GET home page. */
router.get('/', function(req, res, next) {
    console.log(getTime());
    res.json({});
});


router.get('/:id', function(req, res, next) {
    console.log(getTime () + " " + req.params.id);
    res.json({});

});

module.exports = router;
