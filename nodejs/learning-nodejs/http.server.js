var http = require('http');

http.createServer(function(req, res) {
    res.writeHead(200, {'Content-Type' : 'text/plain'});
    res.end('Your console contains a result of this example.\n');

    // Create a general emitter.
    var EventEmitter = require('events').EventEmitter;
    var ee = new EventEmitter();

    // Make a function that will run when an event is called
    function callback_once () {
	console.log('A singular callback.');
	}

    // Another function that will be called.
    function callback_many() {
	console.log('Multiple callback.');
	}

    ee.once("event", callback_once);
    ee.emit("event");
    ee.emit('event');

    console.log('Moving on to calling the multiple callback.');

    ee.on("event", callback_many); // This overwrites the singular callback.  It is not a stack.  It replaces the previous callback.  This is not a stack.
    ee.emit('event');
    ee.emit('event');

    console.log('removing the multiple calls now.');

    ee.removeListener('event', callback_many);
    ee.emit('event');
    })
.listen(8080, 'localhost');

