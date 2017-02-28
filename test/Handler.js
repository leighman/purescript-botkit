var makeMockUser = function (str) {
  return {
    id: str
  }
}

var unsafeShow = JSON.stringify

exports.userShow = unsafeShow
exports.userEq = function (u1) {
  return function (u2) {
    return unsafeShow(u1) === unsafeShow(u2)
  }
}

exports.makeMockUser = makeMockUser

exports.goodStorage = {
  get: function (uid, cb) {
    cb(null, makeMockUser(uid))
  },
  save: function (user, cb) {
    cb(null, user.id)
  },
  all: function (cb) {
    cb(null, [])
  },
}

exports.badStorage = {
  get: function (uid, cb) {
    cb('An error occurred')
  },
  save: function (user, cb) {
    cb('An error occurred')
  },
  all: function (cb) {
    cb('An error occurred')
  }
}

exports.makeMockController = function (storage) {
  return {
    storage: {
      users: storage
    }
  }
}
exports.mockBot = {}
exports.mockMessage = {}
