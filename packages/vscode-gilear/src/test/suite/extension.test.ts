import * as assert from "assert";
// import * as Mocha from "mocha";

// You can import and use all API from the 'vscode' module
// as well as import your extension to test it
import * as vscode from "vscode";
// import * as myExtension from '../extension';

// TODO: run cabal build gilear-lsp before starting tests
suite("Extension Test Suite 1", () => {
  vscode.window.showInformationMessage("Start all tests.");

  // TODO: open and edit test document and test diagnostics
  test("Sample test", () => {
    assert.equal(-1, [1, 2, 3].indexOf(5));
    assert.equal(-1, [1, 2, 3].indexOf(0));
  });
});
