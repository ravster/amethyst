var http = require('http');

var requestListener = function (req, res) {
    res.writeHead(200);
    res.end('Hello HTTP!\n');
    };

var server = http.createServer(requestListener);

server.listen(8080, "localhost");
