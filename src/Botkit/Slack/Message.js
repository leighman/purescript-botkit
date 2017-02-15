exports.messageGetImpl = function (nothing) {
  return function (just) {
    return function (prop) {
      return function (message) {
        if (message[prop]) {
          return just(message[prop])
        } else {
          return nothing
        }
      }
    }
  }
}
