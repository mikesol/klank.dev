exports.chalk = function (c) {
  return function (s) {
    console.log("entering");
    if (
      c.color.constructor.name === "Nothing" &&
      c.bg.constructor.name === "Nothing" &&
      !c.bold &&
      !c.dim &&
      !c.italic &&
      !c.underline &&
      !c.inverse &&
      !c.hidden &&
      !c.strikethrough &&
      !c.visible
    ) {
      return s;
    }
    var $$ = chalk;
    if (c.color.constructor.name === "Just") {
      if (c.color.value0.constructor.name === "CBlack") {
        $$ = $$["black"];
      } else if (c.color.value0.constructor.name === "CRed") {
        $$ = $$["red"];
      } else if (c.color.value0.constructor.name === "CGreen") {
        $$ = $$["green"];
      } else if (c.color.value0.constructor.name === "CYellow") {
        $$ = $$["yellow"];
      } else if (c.color.value0.constructor.name === "CBlue") {
        $$ = $$["blue"];
      } else if (c.color.value0.constructor.name === "CMagenta") {
        $$ = $$["magenta"];
      } else if (c.color.value0.constructor.name === "CCyan") {
        $$ = $$["cyan"];
      } else if (c.color.value0.constructor.name === "CWhite") {
        $$ = $$["white"];
      } else if (c.color.value0.constructor.name === "CBlackBright") {
        $$ = $$["blackBright"];
      } else if (c.color.value0.constructor.name === "CRedBright") {
        $$ = $$["redBright"];
      } else if (c.color.value0.constructor.name === "CGreenBright") {
        $$ = $$["greenBright"];
      } else if (c.color.value0.constructor.name === "CYellowBright") {
        $$ = $$["yellowBright"];
      } else if (c.color.value0.constructor.name === "CBlueBright") {
        $$ = $$["blueBright"];
      } else if (c.color.value0.constructor.name === "CMagentaBright") {
        $$ = $$["magentaBright"];
      } else if (c.color.value0.constructor.name === "CCyanBright") {
        $$ = $$["cyanBright"];
      } else if (c.color.value0.constructor.name === "CWhiteBright") {
        $$ = $$["whiteBright"];
      }
    }
    if (c.bg.constructor.name === "Just") {
      if (c.bg.value0.constructor.name === "BgBlack") {
        $$ = $$["bgBlack"];
      } else if (c.bg.value0.constructor.name === "BgRed") {
        $$ = $$["bgRed"];
      } else if (c.bg.value0.constructor.name === "BgGreen") {
        $$ = $$["bgGreen"];
      } else if (c.bg.value0.constructor.name === "BgYellow") {
        $$ = $$["bgYellow"];
      } else if (c.bg.value0.constructor.name === "BgBlue") {
        $$ = $$["bgBlue"];
      } else if (c.bg.value0.constructor.name === "BgMagenta") {
        $$ = $$["bgMagenta"];
      } else if (c.bg.value0.constructor.name === "BgCyan") {
        $$ = $$["bgCyan"];
      } else if (c.bg.value0.constructor.name === "BgWhite") {
        $$ = $$["bgWhite"];
      } else if (c.bg.value0.constructor.name === "BgBlackBright") {
        $$ = $$["bgBlackBright"];
      } else if (c.bg.value0.constructor.name === "BgRedBright") {
        $$ = $$["bgRedBright"];
      } else if (c.bg.value0.constructor.name === "BgGreenBright") {
        $$ = $$["bgGreenBright"];
      } else if (c.bg.value0.constructor.name === "BgYellowBright") {
        $$ = $$["bgYellowBright"];
      } else if (c.bg.value0.constructor.name === "BgBlueBright") {
        $$ = $$["bgBlueBright"];
      } else if (c.bg.value0.constructor.name === "BgMagentaBright") {
        $$ = $$["bgMagentaBright"];
      } else if (c.bg.value0.constructor.name === "BgCyanBright") {
        $$ = $$["bgCyanBright"];
      } else if (c.bg.value0.constructor.name === "BgWhiteBright") {
        $$ = $$["bgWhiteBright"];
      }
    }
    if (c.bold) {
      console.log("setting to bold");
      $$ = $$["bold"];
    }
    if (c.dim) {
      $$ = $$["dim"];
    }
    if (c.italic) {
      $$ = $$["italic"];
    }
    if (c.underline) {
      $$ = $$["underline"];
    }
    if (c.inverse) {
      $$ = $$["inverse"];
    }
    if (c.hidden) {
      $$ = $$["hidden"];
    }
    if (c.strikethrough) {
      $$ = $$["strikethrough"];
    }
    if (c.visible) {
      $$ = $$["visible"];
    }

    return $$(s);
  };
};
