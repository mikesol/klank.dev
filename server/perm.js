var fs = require("fs");
fs.chmodSync(`./node_modules/.bin/purs`, "755");
fs.chmodSync(`./node_modules/.bin/esbuild`, "755");
