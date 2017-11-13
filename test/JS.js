exports.fruitIdImpl = function(x) { return x; };
exports.v2Impl = function(x, y) { return [x,y]; };
exports.testCB1Impl = function(cb, x) { return cb(x); };
exports.testCB2Impl = function(cb, x, y) { return cb(x,y); };
exports.testCB1CatchingImpl = function(cb, x) {
  try {
    var r = cb('Cucumber');
  } catch (e) {
  }
  return x;
};
