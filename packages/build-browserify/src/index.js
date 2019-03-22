"use strict";

const browserify = require("browserify");

const entryPoint = process.argv[2];

if (entryPoint != null) {
  browserify(entryPoint).bundle(function(err, out) {
    if (err != null) {
      console.error(err);
      process.exitCode = 1;
    } else {
      process.stdout.write(out);
    }
  });
}
