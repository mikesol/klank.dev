var querystring = require("querystring-browser");
exports.getB64 = function (nothing) {
  return function (just) {
    return function () {
      var urlParams = new URLSearchParams(window.location.search);
      var myParam = urlParams.get("b64");
      return myParam ? just(myParam) : nothing;
    };
  };
};
exports.getUrl = function (nothing) {
  return function (just) {
    return function () {
      var urlParams = new URLSearchParams(window.location.search);
      var myParam = urlParams.get("url");
      return myParam ? just(myParam) : nothing;
    };
  };
};
exports.getInitialAccumulator = function (nothing) {
  return function (just) {
    return function () {
      var urlParams = new URLSearchParams(window.location.search);
      var myParam = urlParams.get("acc");
      if (myParam) {
        try {
          return just(JSON.parse(myParam));
        } catch (e) {
          return nothing;
        }
      }
      return nothing;
    };
  };
};
exports.getK = function () {
  var urlParams = new URLSearchParams(window.location.search);
  var myParam = urlParams.get("k");
  return myParam === null || myParam === "false" ? false : true;
};
exports.getNoterm = function () {
  var urlParams = new URLSearchParams(window.location.search);
  var myParam = urlParams.get("noterm");
  return myParam === null || myParam === "false" ? false : true;
};

exports.getC = function () {
  var urlParams = new URLSearchParams(window.location.search);
  var myParam = urlParams.get("c");
  return myParam === null || myParam === "false" ? false : true;
};

exports.getEC = function () {
  var urlParams = new URLSearchParams(window.location.search);
  var myParam = urlParams.get("ec");
  return myParam === null || myParam === "false" ? false : true;
};

exports.escape = function (s) {
  return function () {
    return querystring.escape(s);
  };
};
exports.canvasDimensionHack = function () {
  var canvas = document.getElementById("klank-canvas");
  canvas.width = canvas.clientWidth;
  canvas.height = canvas.clientHeight;
};
exports.serverUrl = function () {
  return process.env.SERVER_URI;
};
exports.firebaseUrl = function () {
  return process.env.FIREBASE_URL;
};
exports.firebaseToken = function () {
  return process.env.FIREBASE_TOKEN;
};
exports.completelyUnsafeEval = function (s) {
  return function () {
    eval(s);
  };
};

exports.stopAudioContext = function (audioCtx) {
  return function () {
    audioCtx.close();
  };
};

exports.loadCustomAudioNodes = function (audioCtx) {
  return function () {
    return audioCtx.audioWorklet.addModule(process.env.AUDIO_MUL_URL);
  };
};
exports.getKlank = function () {
  return window.klank;
};

exports.canvasOrBust = function () {
  var cvs = document.getElementById("klank-canvas");
  if (!cvs) {
    throw new Error("Where's the canvas?");
  }
  return cvs;
};

exports.bufferFromFile = function (ctx) {
  return function (f) {
    return function () {
      var r = new FileReader();
      return new Promise(function (resolve, reject) {
        r.onload = function () {
          resolve(ctx.decodeAudioData(r.result));
        };
        r.onerror = function (e) {
          reject(e);
        };
        r.readAsArrayBuffer(f);
      });
    };
  };
};

exports.getMicrophoneImpl = function () {
  return navigator.mediaDevices.getUserMedia({ audio: true });
};
