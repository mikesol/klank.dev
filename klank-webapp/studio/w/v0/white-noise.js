// white-noise.js
class WhiteNoiseProcessor extends AudioWorkletProcessor {
  static get parameterDescriptors() {
    return [
      {
        name: "gain",
        defaultValue: 1,
        minValue: 0,
        maxValue: 1,
        automationRate: "a-rate",
      },
    ];
  }

  process(inputs, outputs, parameters) {
    const output = outputs[0];
    output.forEach((channel) => {
      for (let i = 0; i < channel.length; i++) {
        channel[i] =
          (Math.random() * 2 - 1) *
          (parameters["gain"].length > 1
            ? parameters["gain"][i]
            : parameters["gain"][0]);
        // note: a parameter contains an array of 128 values (one value for each of 128 samples),
        // however it may contain a single value which is to be used for all 128 samples
        // if no automation is scheduled for the moment.
      }
    });
    return true;
  }
}

registerProcessor("klank-white-noise", WhiteNoiseProcessor);
