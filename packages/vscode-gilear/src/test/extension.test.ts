import { globSync } from "glob";
import * as assert from "assert";
import * as path from "path";
import * as fs from "fs";
// import * as Mocha from "mocha";

// You can import and use all API from the 'vscode' module
// as well as import your extension to test it
import * as vscode from "vscode";
import * as util from "./util";

// TODO: run cabal build gilear-lsp before starting tests
suite("Extension Test Suite", () => {
  suiteSetup(async () => {
    vscode.window.showInformationMessage("Start all tests.");
  });

  test("Is vscode-gilear available?", () => {
    assert.ok(
      vscode.extensions.getExtension("wenkokke.vscode-gilear"),
      "VS Code extension `wenkokke.vscode-gilear` is not available",
    );
  });

  const testDir = path.join(__dirname, "..", "..", "src", "test");
  const testDataDir = path.join(testDir, "data");
  const testSpecDir = path.join(testDir, "spec");
  const testSpecFilePattern = path.join(testSpecDir, "*.test.json");
  const globOptions = { windowsPathsNoEscape: true };
  const testSpecFiles = globSync(testSpecFilePattern, globOptions);

  test("Are there any tests?", () => {
    assert(
      testSpecFiles.length > 0,
      `The glob pattern ${testSpecFilePattern} found no tests`,
    );
  });

  testSpecFiles.forEach((testSpecFile) => {
    const title = `Diagnostic: ${path.basename(testSpecFile, ".test.json")}`;
    test(title, async () => {
      const testSpec = JSON.parse(
        fs.readFileSync(testSpecFile, { encoding: "utf-8" }),
      );
      assert(util.isTestSpec(testSpec));
      const docFile = path.join(testDataDir, testSpec.file);
      const doc = await vscode.workspace.openTextDocument(docFile);
      const editor = await vscode.window.showTextDocument(doc);
      await util.sleep(500);
      await editor.edit((edit) => util.applyEdits(testSpec.edits, edit));
      await util.sleep(500);
      const diagnostics = vscode.languages.getDiagnostics(editor.document.uri);
      // console.log(JSON.stringify(diagnostics));
      for (var index = 0; index < testSpec.diagnostics.length; index += 1) {
        assert(index < diagnostics.length);
        const actual = util.fromDiagnostic(diagnostics[index]);
        const expected = testSpec.diagnostics[index];
        assert.deepStrictEqual(actual, expected);
      }
      await util.sleep(50);
    });
  });
});
