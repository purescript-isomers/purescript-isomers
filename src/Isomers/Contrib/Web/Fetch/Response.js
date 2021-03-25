
exports.json = function (resp) {
  return function() {
    return resp.json();
  };
};
