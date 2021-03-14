var xterm = require("xterm");
var Terminal = xterm.Terminal;
var wxterm = require("xterm-addon-web-links");
var WebLinksAddon = wxterm.WebLinksAddon;

exports.newTerminal = function (theme) {
  return function () {
    var term = new Terminal({
      cursorBlink: true,
      theme,
      rows: 10,
    });
    term.loadAddon(new WebLinksAddon());
    return term;
  };
};
exports.setFontSize = function (s) {
  return function (t) {
    return function () {
      t.setOption("fontSize", s);
    };
  };
};
exports.attachTerminalToElement = function (e) {
  return function (t) {
    return function () {
      t.open(e);
    };
  };
};

exports.monitorForChange = function (t) {
  return function (f) {
    return function () {
      t.onData(function (s) {
        f(s)();
      });
    };
  };
};

exports.writeText = function (s) {
  return function (t) {
    return function () {
      return t.write(s);
    };
  };
};

exports.focus = function (t) {
  return function () {
    return t.focus();
  };
};
