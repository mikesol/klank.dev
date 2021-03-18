var exec = require("child_process").exec;
var rimraf = require("rimraf");

rimraf.sync("ace-builds");
rimraf.sync("chalkie");

console.log("Cloning ace-builds");
exec(
  "git clone --depth 1 --branch v1.4.12 https://github.com/ajaxorg/ace-builds",
  function (err) {
    if (err) {
      console.log(
        "Could not clone ace-builds from https://github.com/ajaxorg/ace-builds. Check to make sure the repo still exists, that your internet is connected and that you have enough space to clone."
      );
      throw err;
    } else {
      console.log("Cloning chalkie");
      exec(
        "git clone --depth 1 --branch v0.0.12 https://github.com/shawwn/chalkie",
        function (err) {
          if (err) {
            console.log(
              "Could not clone chalkie from https://github.com/shawwn/chalkie. Check to make sure the repo still exists, that your internet is connected and that you have enough space to clone."
            );
            throw err;
          }
          console.log("Cloned necessary repos for local development");
        }
      );
    }
  }
);
