exports.copyToClipboard = function () {
    return function () {
      const el = document.getElementById("klank-share-url");
      el.select();
      document.execCommand("copy");
    };
  };
  