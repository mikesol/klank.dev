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
  mixpanel.track("Klank compiled");
  ps.compile({ body: tops })(function (r) {
    return function () {
      console.log("successful completion");
      console.log(r);
      callback(null, {
        statusCode: 200,
        headers: {
          "Access-Control-Allow-Origin": "*",
          "Content-Type": "application/json",
          "Access-Control-Allow-Headers":
            "Content-Type, X-Amz-Date, Authorization, X-Api-Key, X-Amz-Security-Token, X-Amz-User-Agent",
        },
        body: r,
      });
    };
  })();
};
