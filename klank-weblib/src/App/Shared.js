exports.stopAudioContext = function (audioCtx) {
  return function () {
    audioCtx.close();
  };
};
exports.loadCustomAudioNodes = function (audioCtx) {
  return function () {
    var audioMul = `class PsAudMulProcessor extends AudioWorkletProcessor {
  process(inputs, outputs, parameters) {
    if (outputs) {
      for (var i = 0; i < outputs.length; i++) {
        if (outputs[i]) {
          for (var j = 0; j < outputs[i].length; j++) {
            for (var k = 0; k < outputs[i][j].length; k++) {
              outputs[i][j][k] = 1.0;
            }
          }
        }
      }
    }
    if (inputs && inputs[0] && outputs && outputs[0]) {
      for (var i = 0; i < inputs.length; i++) {
        if (inputs[i]) {
          for (
            var j = 0;
            j < Math.min(inputs[i].length, outputs[0].length);
            j++
          ) {
            for (var k = 0; k < inputs[i][j].length; k++) {
              outputs[0][j][k] = outputs[0][j][k] * inputs[i][j][k];
            }
          }
        }
      }
    }
    return true;
  }
}

registerProcessor("ps-aud-mul", PsAudMulProcessor);
    `;
    var audioMulAsBlob = new Blob([audioMul], {
      type: "application/javascript",
    });
    return audioCtx.audioWorklet.addModule(URL.createObjectURL(audioMulAsBlob));
  };
};
exports.getBrowserMediaStreamImpl = function (audio) {
  return function (video) {
    return function () {
      return navigator.mediaDevices.getUserMedia({
        audio: audio,
        video: video,
      });
    };
  };
};
exports.cameraToVideo = function (mediaStream) {
  return function () {
    var video = document.createElement("video");
    video.autoplay = true;
    video.srcObject = mediaStream;
    return video;
  };
};
exports.canvasOrBust = function () {
  var cvs = document.getElementById("klank-canvas");
  if (!cvs) {
    throw new Error("Where's the canvas?");
  }
  return cvs;
};
exports.canvasDimensionHack = function () {
  var canvas = document.getElementById("klank-canvas");
  canvas.width = canvas.clientWidth;
  canvas.height = canvas.clientHeight;
};
