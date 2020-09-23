 function touchAudio(/**dictHomogeneous */) {
    return function (timeToSet) {
      return function (instructions) {
        return function (context) {
          return function (stream) {
            return function (sources) {
              return function (g) {
                return function () {
                  // should never happen
                  if (timeToSet < context.currentTime) {
                    console.warn(
                      "Programming error: we are setting in the past",
                      timeToSet,
                      context.currentTime
                    );
                    timeToSet = context.currentTime;
                  }
                  var generators = g;
                  for (var i = 0; i < instructions.length; i++) {
                    var c = instructions[i];
                    if (c.constructor.name == "DisconnectFrom") {
                      generators[c.value0].disconnect(generators[c.value1]);
                    } else if (c.constructor.name == "ConnectTo") {
                      if (c.value2.constructor.name == "Nothing") {
                        generators[c.value0].connect(generators[c.value1]);
                      } else {
                        generators[c.value0].connect(
                          generators[c.value1],
                          c.value2.value0.value0,
                          c.value2.value0.value1
                        );
                      }
                    } else if (c.constructor.name == "Shuffle") {
                      var old = generators;
                      var generators = new Array(c.value0.length);
                      for (var j = 0; j < c.value0.length; j++) {
                        generators[c.value0[j].value1] = old[c.value0[j].value0];
                      }
                    } else if (c.constructor.name == "NewUnit") {
                      generators[c.value0] =
                        c.value1.constructor.name == "Speaker$prime$prime"
                          ? context.destination
                          : c.value1.constructor.name == "Microphone$prime$prime"
                          ? context.createMediaStreamSource(stream)
                          : c.value1.constructor.name == "Play$prime$prime"
                          ? context.createMediaElementSource(
                              sources[c.value3.value0]
                            )
                          : c.value1.constructor.name == "PlayBuf$prime$prime"
                          ? context.createBufferSource()
                          : c.value1.constructor.name == "LoopBuf$prime$prime"
                          ? context.createBufferSource()
                          : c.value1.constructor.name ==
                            "PlayDynamicBuf$prime$prime"
                          ? context.createBufferSource()
                          : c.value1.constructor.name ==
                            "LoopDynamicBuf$prime$prime"
                          ? context.createBufferSource()
                          : c.value1.constructor.name == "Lowpass$prime$prime"
                          ? context.createBiquadFilter()
                          : c.value1.constructor.name == "Bandpass$prime$prime"
                          ? context.createBiquadFilter()
                          : c.value1.constructor.name == "Lowshelf$prime$prime"
                          ? context.createBiquadFilter()
                          : c.value1.constructor.name == "Highshelf$prime$prime"
                          ? context.createBiquadFilter()
                          : c.value1.constructor.name == "Notch$prime$prime"
                          ? context.createBiquadFilter()
                          : c.value1.constructor.name == "Allpass$prime$prime"
                          ? context.createBiquadFilter()
                          : c.value1.constructor.name == "Peaking$prime$prime"
                          ? context.createBiquadFilter()
                          : c.value1.constructor.name == "Highpass$prime$prime"
                          ? context.createBiquadFilter()
                          : c.value1.constructor.name ==
                            "DynamicConvolver$prime$prime"
                          ? context.createConvolver()
                          : c.value1.constructor.name == "Convolver$prime$prime"
                          ? context.createConvolver()
                          : c.value1.constructor.name ==
                            "DynamicsCompressor$prime$prime"
                          ? context.createDynamicsCompressor()
                          : c.value1.constructor.name == "SawtoothOsc$prime$prime"
                          ? context.createOscillator()
                          : c.value1.constructor.name == "TriangleOsc$prime$prime"
                          ? context.createOscillator()
                          : c.value1.constructor.name == "PeriodicOsc$prime$prime"
                          ? context.createOscillator()
                          : c.value1.constructor.name ==
                            "DynamicPeriodicOsc$prime$prime"
                          ? context.createOscillator()
                          : c.value1.constructor.name ==
                            "DynamicWaveShaper$prime$prime"
                          ? context.createWaveShaper()
                          : c.value1.constructor.name == "WaveShaper$prime$prime"
                          ? context.createWaveShaper()
                          : c.value1.constructor.name == "Dup$prime$prime"
                          ? context.createGain()
                          : c.value1.constructor.name ==
                            "StereoPanner$prime$prime"
                          ? context.createStereoPanner()
                          : c.value1.constructor.name == "SinOsc$prime$prime"
                          ? context.createOscillator()
                          : c.value1.constructor.name == "SquareOsc$prime$prime"
                          ? context.createOscillator()
                          : c.value1.constructor.name == "Mul$prime$prime"
                          ? new AudioWorkletNode(context, "ps-aud-mul")
                          : c.value1.constructor.name == "Add$prime$prime"
                          ? context.createGain()
                          : c.value1.constructor.name == "Delay$prime$prime"
                          ? context.createDelay(10.0) // magic number for 10 seconds...make tweakable?
                          : c.value1.constructor.name == "Constant$prime$prime"
                          ? context.createConstantSource()
                          : c.value1.constructor.name == "Gain$prime$prime"
                          ? context.createGain()
                          : c.value1.constructor.name == "SplitRes$prime$prime"
                          ? context.createGain()
                          : c.value1.constructor.name == "DupRes$prime$prime"
                          ? context.createGain()
                          : c.value1.constructor.name == "Splitter$prime$prime"
                          ? context.createChannelSplitter(c.value2.value0)
                          : c.value1.constructor.name == "Merger$prime$prime"
                          ? context.createChannelMerger(c.value2.value0)
                          : null;
                      if (c.value1.constructor.name == "SinOsc$prime$prime") {
                        generators[c.value0].type = "sine";
                        generators[c.value0].start(timeToSet + c.value4.value0);
                      } else if (
                        c.value1.constructor.name == "LoopBuf$prime$prime"
                      ) {
                        generators[c.value0].loop = true;
                        generators[c.value0].buffer = sources[c.value3.value0];
                        generators[c.value0].start(timeToSet + c.value4.value0);
                      } else if (
                        c.value1.constructor.name == "WaveShaper$prime$prime"
                      ) {
                        generators[c.value0].curve = sources[c.value3.value0];
                      } else if (
                        c.value1.constructor.name == "Convolver$prime$prime"
                      ) {
                        generators[c.value0].buffer = sources[c.value3.value0];
                      } else if (
                        c.value1.constructor.name == "PlayBuf$prime$prime"
                      ) {
                        generators[c.value0].loop = false;
                        generators[c.value0].buffer = sources[c.value3.value0];
                        generators[c.value0].start(timeToSet + c.value4.value0);
                      } else if (
                        c.value1.constructor.name == "LoopDynamicBuf$prime$prime"
                      ) {
                        generators[c.value0].loop = true;
                        generators[c.value0].start(timeToSet + c.value4.value0);
                      } else if (
                        c.value1.constructor.name == "PlayDynamicBuf$prime$prime"
                      ) {
                        generators[c.value0].loop = false;
                        generators[c.value0].start(timeToSet + c.value4.value0);
                      } else if (
                        c.value1.constructor.name == "Play$prime$prime"
                      ) {
                        // todo - add delay somehow...
                        sources[c.value3.value0].play();
                      } else if (
                        c.value1.constructor.name == "Constant$prime$prime"
                      ) {
                        generators[c.value0].start(timeToSet + c.value4.value0);
                      } else if (
                        c.value1.constructor.name == "Lowpass$prime$prime"
                      ) {
                        generators[c.value0].type = "lowpass";
                      } else if (
                        c.value1.constructor.name == "Bandpass$prime$prime"
                      ) {
                        generators[c.value0].type = "bandpass";
                      } else if (
                        c.value1.constructor.name == "Lowshelf$prime$prime"
                      ) {
                        generators[c.value0].type = "lowshelf";
                      } else if (
                        c.value1.constructor.name == "Highshelf$prime$prime"
                      ) {
                        generators[c.value0].type = "highshelf";
                      } else if (
                        c.value1.constructor.name == "Notch$prime$prime"
                      ) {
                        generators[c.value0].type = "notch";
                      } else if (
                        c.value1.constructor.name == "Allpass$prime$prime"
                      ) {
                        generators[c.value0].type = "allpass";
                      } else if (
                        c.value1.constructor.name == "Peaking$prime$prime"
                      ) {
                        generators[c.value0].type = "peaking";
                      } else if (
                        c.value1.constructor.name == "Highpass$prime$prime"
                      ) {
                        generators[c.value0].type = "highpass";
                      } else if (
                        c.value1.constructor.name == "SquareOsc$prime$prime"
                      ) {
                        generators[c.value0].type = "square";
                        generators[c.value0].start(timeToSet + c.value4.value0);
                      } else if (
                        c.value1.constructor.name == "TriangleOsc$prime$prime"
                      ) {
                        generators[c.value0].type = "triangle";
                        generators[c.value0].start(timeToSet + c.value4.value0);
                      } else if (
                        c.value1.constructor.name == "SawtoothOsc$prime$prime"
                      ) {
                        generators[c.value0].type = "sawtooth";
                        generators[c.value0].start(timeToSet + c.value4.value0);
                      } else if (
                        c.value1.constructor.name == "PeriodicOsc$prime$prime"
                      ) {
                        generators[c.value0].type = "custom";
                        generators[c.value0].setPeriodicWave(
                          sources[c.value3.value0]
                        );
                        generators[c.value0].start(timeToSet + c.value4.value0);
                      } else if (
                        c.value1.constructor.name ==
                        "DynamicPeriodicOsc$prime$prime"
                      ) {
                        generators[c.value0].type = "custom";
                        generators[c.value0].start(timeToSet + c.value4.value0);
                      } else if (
                        c.value1.constructor.name == "SplitRes$prime$prime"
                      ) {
                        generators[c.value0].gain.setValueAtTime(1.0, timeToSet);
                      } else if (
                        c.value1.constructor.name == "DupRes$prime$prime"
                      ) {
                        generators[c.value0].gain.setValueAtTime(1.0, timeToSet);
                      }
                    } else if (c.constructor.name == "SetFrequency") {
                      generators[c.value0].frequency.setValueAtTime(
                        c.value1,
                        timeToSet + c.value2
                      );
                    } else if (c.constructor.name == "SetPan") {
                      generators[c.value0].pan.setValueAtTime(
                        c.value1,
                        timeToSet + c.value2
                      );
                    } else if (c.constructor.name == "SetGain") {
                      generators[c.value0].gain.setValueAtTime(
                        c.value1,
                        timeToSet + c.value2
                      );
                    } else if (c.constructor.name == "SetQ") {
                      generators[c.value0].Q.setValueAtTime(
                        c.value1,
                        timeToSet + c.value2
                      );
                    } else if (c.constructor.name == "SetBuffer") {
                      var myArrayBuffer = context.createBuffer(
                        c.value2.length,
                        c.value2[0].length,
                        c.value1
                      );
                      for (
                        var channel = 0;
                        channel < myArrayBuffer.numberOfChannels;
                        channel++
                      ) {
                        var nowBuffering = myArrayBuffer.getChannelData(channel);
                        for (var i = 0; i < myArrayBuffer.length; i++) {
                          nowBuffering[i] = c.value2[channel][i];
                        }
                      }
                      generators[c.value0].buffer = myArrayBuffer;
                    } else if (c.constructor.name == "SetDelay") {
                      generators[c.value0].delayTime.setValueAtTime(
                        c.value1,
                        timeToSet + c.value2
                      );
                    } else if (c.constructor.name == "SetOffset") {
                      generators[c.value0].offset.setValueAtTime(
                        c.value1,
                        timeToSet + c.value2
                      );
                    } else if (c.constructor.name == "SetLoopStart") {
                      generators[c.value0].loopStart.setValueAtTime(
                        c.value1,
                        timeToSet + c.value2
                      );
                    } else if (c.constructor.name == "SetLoopEnd") {
                      generators[c.value0].loopEnd.setValueAtTime(
                        c.value1,
                        timeToSet + c.value2
                      );
                    } else if (c.constructor.name == "SetOversample") {
                      generators[c.value0].oversample = c.value1;
                    } else if (c.constructor.name == "SetCurve") {
                      var curve = new Float32Array(c.value1.length);
                      for (var i = 0; i < c.value1.length; i++) {
                        curve[i] = c.value1[i];
                      }
  
                      generators[c.value0].curve = curve;
                    } else if (c.constructor.name == "SetPeriodicWave") {
                      var real = new Float32Array(c.value1.length);
                      var imag = new Float32Array(c.value2.length);
                      for (var i = 0; i < c.value1.length; i++) {
                        real[i] = c.value1[i];
                        imag[i] = c.value2[i];
                      }
  
                      var wave = context.createPeriodicWave(real, imag, {
                        disableNormalization: true,
                      });
  
                      generators[c.value0].setPeriodicWave(wave);
                    } else if (c.constructor.name == "SetPlaybackRate") {
                      generators[c.value0].playbackRate.setValueAtTime(
                        c.value1,
                        timeToSet + c.value2
                      );
                    } else if (c.constructor.name == "SetThreshold") {
                      generators[c.value0].threshold.setValueAtTime(
                        c.value1,
                        timeToSet + c.value2
                      );
                    } else if (c.constructor.name == "SetKnee") {
                      generators[c.value0].knee.setValueAtTime(
                        c.value1,
                        timeToSet + c.value2
                      );
                    } else if (c.constructor.name == "SetRatio") {
                      generators[c.value0].ratio.setValueAtTime(
                        c.value1,
                        timeToSet + c.value2
                      );
                    } else if (c.constructor.name == "SetAttack") {
                      generators[c.value0].attack.setValueAtTime(
                        c.value1,
                        timeToSet + c.value2
                      );
                    } else if (c.constructor.name == "SetRelease") {
                      generators[c.value0].release.setValueAtTime(
                        c.value1,
                        timeToSet + c.value2
                      );
                    } else if (c.constructor.name == "Stop") {
                      generators[c.value0].stop();
                    }
                  }
                  return generators;
                };
              };
            };
          };
        };
      };
    };
  };