var express = require("express");
var bodyParser = require("body-parser");
var ps = require("./output/Main/");
var helmet = require("helmet");
var cors = require("cors");
var app = express();
var fs = require("fs");
var libcurl = require("node-libcurl");
var Curl = libcurl.Curl;
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
  const curl = new Curl();
  const close = curl.close.bind(curl);

  curl.setOpt(Curl.option.URL, "http://0x0.st");
  curl.setOpt(Curl.option.HTTPPOST, [
    { name: "file", file: fn, type: "text/plain" },
  ]);

  curl.on("end", function (s, d) {
    res.send(d.split("\n")[0]);
    fs.unlinkSync(fn);
    close();
  });
  curl.on("error", function () {
    res.status(400).send();
    fs.unlinkSync(fn);
    close();
  });
  curl.perform();
});

app.listen(process.env.PORT || 3000, () => {
  console.log("Server started");
});
