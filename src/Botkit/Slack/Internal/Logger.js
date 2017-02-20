exports.error = function (c) {
  return function (s) {
    return function () {
      c.log.error(s)
    }
  }
}

exports.warn = function (c) {
  return function (s) {
    return function () {
      c.log.warn(s)
    }
  }
}

exports.info = function (c) {
  return function (s) {
    return function () {
      c.log.info(s)
    }
  }
}

exports.debug = function (c) {
  return function (s) {
    return function () {
      c.log.debug(s)
    }
  }
}
