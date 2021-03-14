var Mixpanel = require("mixpanel");
var AWS = require("aws-sdk");
var s3 = new AWS.S3();
var mixpanel;
if (process.env.NODE_ENV !== "development") {
  mixpanel = Mixpanel.init(process.env.MIXPANEL_TOKEN, {
    host: "api-eu.mixpanel.com",
  });
}

exports.handler = function (event, context, callback) {
  if (!event.body) {
    throw new Error("Need a body.");
  }
  var tops = JSON.parse(event.body);
  if (!tops.code) {
    throw new Error("Need code.");
  }
  var notPurs =
    tops.code.indexOf("module") < 0 ||
    tops.code.indexOf("main") < 0 ||
    tops.code.indexOf("runInBrowser") < 0;
  var notAudioProcessor =
    tops.code.indexOf("AudioWorkletProcessor") < 0 ||
    tops.code.indexOf("process") < 0 ||
    tops.code.indexOf("registerProcessor") < 0;
  if (notPurs && notAudioProcessor) {
    throw new Error("invalid input");
  }
  if (process.env.NODE_ENV !== "development") {
    mixpanel.track(notAudioProcessor ? "Klank saved" : "Processor saved");
  }
  var stream = Buffer.from(tops.code, "binary");
  var o =
    "K" +
    new Date().getTime() +
    "" +
    Math.floor(Math.random() * 10000) +
    (notPurs ? ".js" : ".purs");
  var params = {
    ACL: "public-read",
    Bucket: "klank-share",
    ContentType: notPurs ? "application/javascript" : "text/plain",
    Key: o,
    Body: stream,
  };
  s3.upload(params, function (err, data) {
    if (err) {
      console.log(err);
      callback("Could not upload file.");
    } else {
      callback(null, {
        statusCode: 200,
        headers: {
          "Access-Control-Allow-Origin": "*",
          "Content-Type": "text/plain",
          "Access-Control-Allow-Headers":
            "Content-Type, X-Amz-Date, Authorization, X-Api-Key, X-Amz-Security-Token, X-Amz-User-Agent",
        },
        body: data.Location,
      });
    }
  });
};
