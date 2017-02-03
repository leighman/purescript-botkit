exports.startRTMImpl = function (bot) {
  return function (onError) {
    return function () {
      return bot.startRTM(function (err) {
        if (err) onError(err)
      })
    }
  }
}
