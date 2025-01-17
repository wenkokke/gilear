import { globSync } from "glob";
import * as assert from "assert";
import * as path from "path";
import * as vscode from "vscode";
import * as Gilear from "../extension";
import { Extension } from "vscode";
import { GoldenTest } from "./GoldenTest";

// NOTE(directory-structure): this code depends on the directory that contains the test data
const testDir = path.join(__dirname, "..", "..", "src", "test");
const goldenTestCasesDir = path.join(testDir, "golden");
const goldenTestFilesDir = path.join(goldenTestCasesDir, "files");
const goldenTestCasePattern = path.join(
  goldenTestCasesDir,
  `*${GoldenTest.fileExt}`,
);
const goldenTestCaseOptions = { windowsPathsNoEscape: true };

// TODO: run cabal build gilear-lsp before starting tests
suite("Extension Test Suite", () => {
  suiteSetup(async () => {
    vscode.window.showInformationMessage("Start all tests.");
  });

  test(`Is '${Gilear.extensionId}' available?`, async () => {
    const extensionAPI = await Gilear.extensionAPI();
    assert.ok(
      extensionAPI,
      `VS Code extension '${Gilear.extensionId}' is not available`,
    );
    assert(
      extensionAPI.client.isRunning(),
      `VS Code extension '${Gilear.extensionId}' is not running`,
    );
  });

  // Run golden file tests:
  const goldenTestCaseFiles = globSync(
    goldenTestCasePattern,
    goldenTestCaseOptions,
  );

  test("Are there any golden tests?", () => {
    assert(
      goldenTestCaseFiles.length > 0,
      `The glob pattern ${goldenTestCasePattern} found no tests`,
    );
  });

  goldenTestCaseFiles.sort().forEach((testCaseFile) => {
    const name = path.basename(testCaseFile, GoldenTest.fileExt);
    const title = `Test: ${name}`;
    test(title, async () => {
      // Track whether or not the test case has change
      const testCase = GoldenTest.fromFile(testCaseFile);
      assert.ok(testCase);
      await Gilear.extensionAPI();
      // Run the test case:
      await testCase.assertSuccess(goldenTestCasesDir, goldenTestFilesDir);
    });
  });
});
