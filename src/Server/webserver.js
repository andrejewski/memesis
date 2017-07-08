var http = require('http')
var uuid = require('uuid')

var store = {}

Elm.ports.httpRequest = F2(function (port, callback) {
  var server = http.createServer(function (request, response) {
    var context = uuid.v4()
    var body = ''

    store[context] = response

    req.on('data', function (chunk) {
      body += chunk
    })
    req.on('end', function () {
      var req = {
        method: req.method,
        url: req.url,
        headers: req.headers,
        cookies: req.cookies,
      }
      A2(callback, context, req)
    })
  })

  server.listen(port)

  return function () {
    server.close()
  }
})

Elm.ports.httpRespond = F2(function (context, res) {
  var response = store[context]

  Object.keys(res.headers).forEach(function (key) {
    repsonse.setHeader(key, res.headers[key])
  })

  response.statusCode = res.statusCode
  response.statusMessage = res.statusMessage
  response.write(res.body)
  response.end()
})
