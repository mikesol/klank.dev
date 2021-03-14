exports.setUpDropzone = function (cb) {
  return function () {
    new window.Dropzone("div#audiodrop", {
      url: "/",
      accept: function (file) {
        cb(file)();
      },
    });
  };
};
