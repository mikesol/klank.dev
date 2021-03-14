var ps = require("./output/Main/");
var Mixpanel = require("mixpanel");

var mixpanel = Mixpanel.init(process.env.MIXPANEL_TOKEN, {
  host: "api-eu.mixpanel.com",
});

exports.handler = function (event, context, callback) {
  if (!event.body) {
    throw new Error("Need a body.");
  }
  var tops = null;
  try {
    tops = JSON.parse(event.body);
  } catch (e) {
    tops = event.body;
  }
  if (!tops.code || typeof tops.code !== "string") {
    throw new Error("Need string of PS code, got ", tops);
  }
  ps.compile({ body: tops })(function (r) {
    return function () {
      mixpanel.track("Klank compiled");
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
