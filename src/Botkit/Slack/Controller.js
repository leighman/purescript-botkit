exports.createSlackBotImpl = function (config) {
  var Botkit = require('botkit')
  return function () {
    return Botkit.slackbot(config)
  }
}

exports.toSlackAppImpl = function (config) {
  return function (controller) {
    return function () {
      return controller.configureSlackApp(config)
    }
  }
}

exports.setupWebserverImpl = function (controller, port, onError, onSuccess) {
  return function () {
    controller.setupWebserver(port, function (err, webserver) {
      if (err) {
        onError(Error(err))()
      } else {
        onSuccess(webserver)()
      }
    })
  }
}

exports.createWebhookEndpointsImpl = function (controller) {
  return function (webserver) {
    return function () {
      controller.createWebhookEndpoints(webserver)
    }
  }
}

exports.createOauthEndpointsImpl = function (controller, webserver, onError, onSuccess) {
  return function () {
    controller.createOauthEndpoints(webserver, function (err, req, res) {
      if (err) {
        onError(Error(err))(req)(res)()()
      } else {
        onSuccess(req)(res)()()
      }
    })
  }
}
