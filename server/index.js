var express = require("express");
var bodyParser = require("body-parser");
var ps = require("./output/Main/");
var helmet = require("helmet");
var cors = require("cors");
var app = express();
var fs = require("fs");
var AWS = require("aws-sdk");
app.use(cors());
app.use(helmet());
var s3 = new AWS.S3();

app.post("/", bodyParser.json(), function (req, res) {
  if (!req.body.code) {
    throw new Error("Need a code param");
  }
  ps.compile(req)(function (r) {
    return function () {
      res.json(JSON.parse(r));
    };
  })();
});

app.post("/u", bodyParser.json(), function (req, res) {
  if (!req.body.code) {
    throw new Error("Need a code param");
  }
  if (
    req.body.code.indexOf("module") < 0 ||
    req.body.code.indexOf("main") < 0 ||
    req.body.code.indexOf("runInBrowser") < 0
  ) {
    throw new Error("invalid input");
  }
  var stream = Buffer.from(req.body.code, "binary");
  var o =
    new Date().getTime() + "" + Math.floor(Math.random() * 10000) + ".purs";
  var params = {
    ACL: "public-read",
    Bucket: "klank-share",
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
});

app.listen(process.env.PORT || 3000, () => {
  console.log("Server started");
});
