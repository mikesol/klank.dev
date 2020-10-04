exports.serverUrl = function () {
  return process.env.SERVER_URI;
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
var cbHack = function (res) {
  return function (rej) {
    return res({});
  };
};
var cbAHack = function (res) {
  return function (rej) {
    return res([]);
  };
};
exports.getKlank = function () {
  if (!window.klank.enableMicrophone) {
    window.klank.enableMicrophone = false;
  }
  if (!window.klank.accumulator) {
    window.klank.accumulator = cbHack;
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
