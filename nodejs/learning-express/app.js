var express = require('express');
var app = express();

// Defining a route over here, with a callback that will handle requests to that route.
app.get('/hello.txt', function (req, res) {
    res.send('hello world :D:)');
});

// Defining another route here.  use() works on '/' by default.
// next() just allows the system to finish the current callback and then continue on down the list of route handlers just in case there is an additional one that matches too.
app.use(function(req,res,next){
    console.log(req.method, req.url);
    next();
});

app.use(function(req,res){
    res.send('hello foo');
});

app.listen(8080);
console.log('listening on port 8080.');

