// Readline lets us tap into the process events
const readline = require("readline");

// Allows us to listen for events from stdin
readline.emitKeypressEvents(process.stdin);

let keystrokeBuffer = [];
let needingKeys = [];

// Raw mode gets rid of standard keypress events and other
// functionality Node.js adds by default
process.stdin.setRawMode(true);

// Start the keypress listener for the process
process.stdin.on("keypress", (str, key) => {
  if (needingKeys.length) {
    needingKeys.pop()(key);
  } else {
    keystrokeBuffer.push(key);
  }

  // "Raw" mode so we must do our own kill switch
  if (key.sequence === "\u0003") {
    process.exit();
  }

  // User has triggered a keypress, now do whatever we want!
  // ...
});

exports.exitProcess = function(error, success) {
  console.log("Goodbye!");
  process.exit();

  return function(a, b, c) {
    c();
  };
};

exports.getNextKey = function(error, success) {
  if (keystrokeBuffer.length) {
    success(keystrokeBuffer.shift());
  } else {
    needingKeys.push(success);
  }

  return function(a, b, cancelSuccess) {
    needingKeys = needingKeys.filter(function(x) {
      return x !== success;
    });
    cancelSuccess();
  };
};
