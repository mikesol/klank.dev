var querystring = require("querystring-browser");
var IPFS = require("ipfs");
exports.getB64 = function (nothing) {
  return function (just) {
    return function () {
      var urlParams = new URLSearchParams(window.location.search);
      var myParam = urlParams.get("b64");
      return myParam ? just(myParam) : nothing;
    };
  };
};
exports.getIPFS = function (nothing) {
  return function (just) {
    return function () {
      var urlParams = new URLSearchParams(window.location.search);
      var myParam = urlParams.get("ipfs");
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

exports.ipfsPut = function (s) {
  return function () {
    let n;
    return IPFS.create()
      .then(function (node) {
        n = node;
        return node.add(s);
      })
      .then(function (results) {
        n.stop();
        return results.path;
      });
  };
};

exports.ipfsGet = function (s) {
  return function () {
    let n;
    var piter = function (r) {
      if (r.res.done) {
        n.stop();
        return r.data + (r.res && r.res.value ? r.res.value.toString() : "");
      }
      return r.iter.next().then(function (res) {
        return piter({
          res: res,
          iter: r.iter,
          data: r.data + r.res.value.toString(),
        });
      });
    };
    return IPFS.create().then(function (node) {
      n = node;
      var iter = node.cat(s);
      return iter.next().then(function (res) {
        return piter({
          iter: iter,
          res: res,
          data: "",
        });
      });
    });
  };
};
