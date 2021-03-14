exports.toJpegDataUrl = function (quality) {
  return function (c) {
    return function () {
      return c.toDataURL("image/jpeg", quality);
    };
  };
};

exports.imageStringToImage = function (s) {
  return function () {
    var i = new Image();
    i.src = s;
    return i;
  };
};
