var express = require("express");
var bodyParser = require("body-parser");
var ps = require("./output/Main/");
var helmet = require("helmet");
var cors = require("cors");
var app = express();
var fs = require("fs");
var child_process = require("child_process");

app.use(cors());
app.use(helmet());
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

app.post("/0x0", bodyParser.json(), function (req, res) {
  if (!req.body.code) {
    throw new Error("Need a code param");
  }
  var fn = new Date().getTime() + "" + Math.random() + ".txt";
  fs.writeFileSync(fn, req.body.code);
  child_process.exec("curl -F file=@" + fn + " http://0x0.st", function (a, b) {
    if (a !== null) {
      res.status(400);
      res.send("bad request");
    } else {
      res.send(b);
    }
    fs.unlinkSync(fn);
  });
});

app.listen(process.env.PORT || 3000, () => {
  console.log("Server started");
});
