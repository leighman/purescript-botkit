exports.toRawAttachmentImpl = function (unwrap) {
  return function (vals) {
    var attachment = {}
    for (var i = 0; i < vals.length; i++) {
      attachment[vals[i].k] = unwrap(vals[i].v)
    }
    return attachment
  }
}

exports.renderConfirmImpl = function (confirm) {
  confirm.ok_text = confirm.okText
  confirm.dismiss_text = confirm.dismissText
  delete confirm.okText
  delete confirm.dismissText
  return confirm
}
