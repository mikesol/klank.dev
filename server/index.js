var express = require("express");
var bodyParser = require("body-parser");
var ps = require("./output/Main/");
var helmet = require("helmet");
var cors = require("cors");
var AWS = require("aws-sdk");
var Mixpanel = require("mixpanel");

var app = express();
app.use(cors());
app.use(helmet());
var s3 = new AWS.S3();
var mixpanel = Mixpanel.init(process.env.MIXPANEL_TOKEN, {
  host: "api-eu.mixpanel.com",
});

app.post(
  "/",
  bodyParser.json({
    limit: "10mb",
  }),
  function (req, res) {
    if (!req.body.code) {
      throw new Error("Need a code param");
    }
    mixpanel.track("Klank compiled");
    ps.compile(req)(function (r) {
      return function () {
        res.json(JSON.parse(r));
      };
    })();
  }
);

app.post(
  "/u",
  bodyParser.json({
    limit: "10mb",
  }),
  function (req, res) {
    if (!req.body.code) {
      throw new Error("Need a code param");
    }
    var notPurs =
      req.body.code.indexOf("module") < 0 ||
      req.body.code.indexOf("main") < 0 ||
      req.body.code.indexOf("runInBrowser") < 0;
    var notAudioProcessor =
      req.body.code.indexOf("AudioWorkletProcessor") < 0 ||
      req.body.code.indexOf("process") < 0 ||
      req.body.code.indexOf("registerProcessor") < 0;
    if (notPurs && notAudioProcessor) {
      throw new Error("invalid input");
    }
    mixpanel.track(notAudioProcessor ? "Klank saved" : "Processor saved");
    var stream = Buffer.from(req.body.code, "binary");
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
        res.status(400).send("Server error");
      } else {
        res.send(data.Location);
      }
    });
  }
);

app.post(
  "/uk",
  bodyParser.json({
    limit: "10mb",
  }),
  function (req, res) {
    if (!req.body.code) {
      throw new Error("Need a code param");
    }
    var notKlank = req.body.code.indexOf("periodicWaves") < 0 ||
      req.body.code.indexOf("floatArrays") < 0 ||
      req.body.code.indexOf("buffers") < 0 ||
      req.body.code.indexOf("exporter") < 0 ||
      req.body.code.indexOf("engineInfo") < 0 ||
      req.body.code.indexOf("enableMicrophone") < 0 ||
      req.body.code.indexOf("tracks") < 0 ||
      req.body.code.indexOf("accumulator") < 0 ||
      req.body.code.indexOf("run") < 0 ||
      req.body.code.indexOf("recorders") < 0;
    if (notKlank) {
      throw new Error("invalid input");
    }
    mixpanel.track("Compiled klank saved");
    var stream = Buffer.from(req.body.code, "binary");
    var o =
      "klank" +
      new Date().getTime() +
      "" +
      Math.floor(Math.random() * 10000) +
      ".js";
    var params = {
      ACL: "public-read",
      Bucket: "klank-share",
      ContentType:  "application/javascript",
      Key: o,
      Body: stream,
    };
    s3.upload(params, function (err, data) {
      if (err) {
        console.log(err);
        res.status(400).send("Server error");
      } else {
        res.send(data.Location);
      }
    });
  }
);

app.listen(process.env.PORT || 3000, () => {
  console.log("Server started");
});
