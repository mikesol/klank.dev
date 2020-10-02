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

exports.getKlank = function (affer) {
  return function () {
    if (!window.klank.enableMicrophone) {
      window.klank.enableMicrophone = false;
    }
    if (!window.klank.accumulator) {
      window.klank.accumulator = affer({});
    }
    if (!window.klank.tracks) {
      window.klank.tracks = affer({});
    }
    if (!window.klank.buffers) {
      window.klank.buffers = function () {
        return affer({});
      };
    }
    if (!window.klank.floatArrays) {
      window.klank.floatArrays = affer({});
    }
    if (!window.klank.periodicWaves) {
      window.klank.periodicWaves = function () {
        return affer({});
      };
    }
    return window.klank;
  };
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
