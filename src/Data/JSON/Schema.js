"use strict";

exports.writeYaml = function (value) {
  const yaml = require("js-yaml")
  return yaml.dump(value)
}

exports.stringify = function (f) { return JSON.stringify(f) }

exports.foreignEq = function(a) { return function(b) { return a === b } }