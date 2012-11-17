// Sending out a query string

var querystring = require('querystring');
var http = require('http');

// stringify() adds the & and =
var post_data = querystring.stringify(
    {
	'query' : "bananas",
	"region" : "Brazil"
	});
var postOptions = {
    host : "google.com",
    path : "/search",
    method : "GET",
    headers : {
	'Content-Type' : 'application/x-www-form-urlencoded',
	'Content-Length' : post_data.length
	}
    };

var postRequest = http.request(postOptions, function(result){
    result.setEncoding('utf8');
    result.on('data', function(chunk){
	console.log("Response: " + chunk);
	});
    });

// write parameters to post body (?)
postRequest.write(post_data);
postRequest.end();

// I don't thoroughly understand what is happening here.
