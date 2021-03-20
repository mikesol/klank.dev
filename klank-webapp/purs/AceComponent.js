exports.windowHack = function () {
  window.dispatchEvent(new Event("resize"));
};
