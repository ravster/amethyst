// Episode 1 of nodetuts

var http = require('http');

var server = http.createServer();

function handleAnyRequest(req, res) {
  res.writeHead(200, {'content-type':'text/plain'});
  res.write('Hello World!');
  res.end();			// Very important.  Server will not send out the response till this happens, so nothing will show on the browser.
}

server.on('request', handleAnyRequest);

server.listen(3000);
