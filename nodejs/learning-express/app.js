var express = require('express');
var app = express();

// Defining a route over here, with a callback that will handle requests to that route.
app.get('/hello.txt', function (req, res) {
    res.send('hello world :D:)');
});

app.listen(8080);
console.log('listening on port 8080.');

