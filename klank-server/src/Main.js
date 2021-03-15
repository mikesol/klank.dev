var rimraf = require("rimraf");
exports.clearDeps = function () {
  rimraf.sync("/tmp/deps/");
};
