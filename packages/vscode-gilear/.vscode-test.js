const assert = require("assert");
const { defineConfig } = require("@vscode/test-cli");
const { ConsoleLogger } = require("./out/extension/logger/ConsoleLogger.js");
const { findExecutable } = require("./out/extension/findExecutable.js");

const GILEAR_LSP_PATH = findExecutable(ConsoleLogger);
assert(typeof GILEAR_LSP_PATH === "string");

module.exports = defineConfig({
  files: "out/test/**/*.test.js",
  mocha: {
    ui: "tdd",
    timeout: 20_000,
  },
  env: { ...process.env, GILEAR_LSP_PATH },
});
