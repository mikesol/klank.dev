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
    var notJs =
      req.body.code.indexOf("AudioWorkletProcessor") < 0 ||
      req.body.code.indexOf("process") < 0 ||
      req.body.code.indexOf("registerProcessor") < 0;
    if (notPurs && notJs) {
      throw new Error("invalid input");
    }
    mixpanel.track(notJs ? "Klank saved" : "Processor saved");
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

app.listen(process.env.PORT || 3000, () => {
  console.log("Server started");
});
