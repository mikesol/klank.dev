var express = require("express");
var bodyParser = require("body-parser");
var ps = require("./output/Main/");
var helmet = require("helmet");
var cors = require('cors');
var app = express();

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

app.listen(process.env.PORT || 3000, () => {
  console.log("Server started");
});
