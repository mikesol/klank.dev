var ps = require("./output/Main/");
var Mixpanel = require("mixpanel");

var mixpanel = Mixpanel.init(process.env.MIXPANEL_TOKEN, {
  host: "api-eu.mixpanel.com",
});

exports.handler = function (event, context, callback) {
  if (!event.body) {
    throw new Error("Need a body.");
  }
  var tops = JSON.parse(event.body);
  if (!tops.code) {
    throw new Error("Need code.");
  }
  var notKlank =
    tops.code.indexOf("periodicWaves") < 0 ||
    tops.code.indexOf("floatArrays") < 0 ||
    tops.code.indexOf("buffers") < 0 ||
    tops.code.indexOf("exporter") < 0 ||
    tops.code.indexOf("engineInfo") < 0 ||
    tops.code.indexOf("enableMicrophone") < 0 ||
    tops.code.indexOf("tracks") < 0 ||
    tops.code.indexOf("accumulator") < 0 ||
    tops.code.indexOf("run") < 0 ||
    tops.code.indexOf("recorders") < 0;
  if (notKlank) {
    throw new Error("invalid input");
  }
  mixpanel.track("Compiled klank saved");
  var stream = Buffer.from(tops.code, "binary");
  var o =
    "klank" +
    new Date().getTime() +
    "" +
    Math.floor(Math.random() * 10000) +
    ".js";
  var params = {
    ACL: "public-read",
    Bucket: "klank-share",
    ContentType: "application/javascript",
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
