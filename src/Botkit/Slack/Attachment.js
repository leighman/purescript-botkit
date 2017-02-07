exports.toRawAttachmentImpl = function (unwrap) {
  return function (vals) {
    var attachment = {}
    for (var i = 0; i < vals.length; i++) {
      attachment[vals[i].k] = unwrap(vals[i].v)
    }
    return attachment
  }
}
