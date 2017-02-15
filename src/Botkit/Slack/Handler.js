exports.replyAcknowledgeImpl = function (bot) {
  return function () {
    bot.replyAcknowledge()
  }
}

exports.replyTypeImpl = function (replyType) {
  return function (bot) {
    return function (message) {
      return function (reply) {
        return function () {
          bot[replyType](message, reply)
        }
      }
    }
  }
}
