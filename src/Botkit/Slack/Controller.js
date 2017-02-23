exports.createSlackBotImpl = function (toNullable) {
  return function (config) {
    var Botkit = require('botkit')
    var c = {}
    for (var k in config) {
      var x = toNullable(config[k])
      if (x) c[k] = x
    }

    return function () {
      return Botkit.slackbot(c)
    }
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

exports.spawnImpl = function (controller) {
  return function (config) {
    return function () {
      return controller.spawn(config)
    }
  }
}

exports.onImpl = function () {
  return function (controller, events, handler) {
    return function () {
      var f = function (b, m) {
        return handler(b)(m)()
      }
      controller.on(events, f)
    }
  }
}

exports.hearsImpl = function () {
  return function (controller, patterns, events, handler) {
    return function () {
      var f = function (b, m) {
        return handler(b)(m)()
      }
      controller.hears(patterns, events, f)
    }
  }
}
