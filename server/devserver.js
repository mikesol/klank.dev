var express = require("express");
var bodyParser = require("body-parser");
var compile = require("./compile");
var ufile = require("./ufile");
var uklank = require("./uklank");
var helmet = require("helmet");
var cors = require("cors");

var app = express();
app.use(cors());
app.use(helmet());

function asText(handler, req, res) {
  handler(req, null, function (err, r) {
    if (err) {
      throw err;
    }
    res.send(r.body);
  });
}

function asJSON(handler, req, res) {
  handler(req, null, function (err, r) {
    //console.log("R", r);
    if (err) {
      throw err;
    }
    res.json(JSON.parse(r.body));
  });
}

app.post(
  "/compile",
  bodyParser.json({
    limit: "10mb",
  }),
  function (req, res) {
    asJSON(compile.handler, req, res);
  }
);

app.post(
  "/u",
  bodyParser.json({
    limit: "10mb",
  }),
  function (req, res) {
    asText(ufile.handler, req, res);
  }
);
app.post(
  "/uk",
  bodyParser.json({
    limit: "10mb",
  }),
  function (req, res) {
    asText(uklank.handler, req, res);
  }
);
var port = process.env.PORT || 3000;
app.listen(port, () => {
  console.log("Server started on port", port);
});
