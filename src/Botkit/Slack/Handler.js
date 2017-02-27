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

exports.getImpl = function (controller) {
  return function (type) {
    return function (xid) {
      return function (onError) {
        return function (onSuccess) {
          return function () {
            controller.storage[type].get(xid, function (err, x) {
              if (err) {
                onError(Error(err))()
              } else {
                onSuccess(x)()
              }
            })
          }
        }
      }
    }
  }
}

exports.saveOrDeleteImpl = function (controller) {
  return function (type) {
    return function (saveOrDelete) {
      return function (x) {
        return function (onError) {
          return function (onSuccess) {
            return function () {
              controller.storage[type][saveOrDelete](x, function (err) {
                if (err) {
                  onError(Error(err))()
                } else {
                  onSuccess()()
                }
              })
            }
          }
        }
      }
    }
  }
}

exports.allImpl = function (controller) {
  return function (type) {
    return function (onError) {
      return function (onSuccess) {
        return function () {
          controller.storage[type].all(function (err, xs) {
            if (err) {
              onError(Error(err))()
            } else {
              onSuccess(xs)()
            }
          })
        }
      }
    }
  }
}
