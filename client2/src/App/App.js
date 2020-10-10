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
exports.getK = function () {
  var urlParams = new URLSearchParams(window.location.search);
  var myParam = urlParams.get("k");
  return myParam === null || myParam === "false" ? false : true;
};

exports.escape = function (s) {
  return function () {
    return querystring.escape(s);
  };
};
exports.copyToClipboard = function (str) {
  return function () {
    const el = document.createElement("textarea");
    el.value = str;
    el.setAttribute("readonly", "");
    el.style.position = "absolute";
    el.style.left = "-9999px";
    document.body.appendChild(el);
    el.select();
    document.execCommand("copy");
    document.body.removeChild(el);
  };
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
var cbHack = function () {
  return function (res) {
    return function (rej) {
      return res({});
    };
  };
};
var cbAHack = function () {
  return function (res) {
    return function (rej) {
      return res([]);
    };
  };
};
exports.getKlank = function () {
  if (!window.klank.enableMicrophone) {
    window.klank.enableMicrophone = false;
  }
  if (!window.klank.accumulator) {
    window.klank.accumulator = function (res) {
      return function (rej) {
        return res([]);
      };
    };
  }
  if (!window.klank.tracks) {
    window.klank.tracks = cbHack;
  }
  if (!window.klank.buffers) {
    window.klank.buffers = function () {
      return cbHack;
    };
  }
  if (!window.klank.worklets) {
    window.klank.worklets = cbAHack;
  }
  if (!window.klank.floatArrays) {
    window.klank.floatArrays = cbHack;
  }
  if (!window.klank.periodicWaves) {
    window.klank.periodicWaves = function () {
      return cbHack;
    };
  }
  return window.klank;
};

exports.canvasOrBust = function () {
  var cvs = document.getElementById("klank-canvas");
  if (!cvs) {
    throw new Error("Where's the canvas?");
  }
  return cvs;
};

exports.getMicrophoneImpl = function () {
  return navigator.mediaDevices.getUserMedia({ audio: true });
};
