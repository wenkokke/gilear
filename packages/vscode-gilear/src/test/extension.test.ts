import { globSync } from "glob";
import * as assert from "assert";
import * as path from "path";
import * as vscode from "vscode";
import * as Gilear from "../extension";
import { Extension } from "vscode";
import { TestCase } from "./TestCase";

// NOTE(directory-structure): this code depends on the directory that contains the test data
const testDir = path.join(__dirname, "..", "..", "src", "test");
const testCasesDir = path.join(testDir, "golden");
const testFilesDir = path.join(testCasesDir, "files");
const testCasePattern = path.join(testCasesDir, `*${TestCase.fileExt}`);
const testCaseOptions = { windowsPathsNoEscape: true };

// TODO: run cabal build gilear-lsp before starting tests
suite("Extension Test Suite", () => {
  suiteSetup(async () => {
    vscode.window.showInformationMessage("Start all tests.");
  });

  test(`Is '${Gilear.extensionId}' available?`, async () => {
    const ext = await activatedExtension();
    assert.ok(
      ext,
      `VS Code extension '${Gilear.extensionId}' is not available`,
    );
    assert(
      ext.exports.client.isRunning(),
      `VS Code extension '${Gilear.extensionId}' is not running`,
    );
  });

  // Run golden file tests:
  const testCaseFiles = globSync(testCasePattern, testCaseOptions);

  test("Are there any golden tests?", () => {
    assert(
      testCaseFiles.length > 0,
      `The glob pattern ${testCasePattern} found no tests`,
    );
  });

  testCaseFiles.sort().forEach((testCaseFile) => {
    const name = path.basename(testCaseFile, TestCase.fileExt);
    const title = `Test: ${name}`;
    test(title, async () => {
      // Track whether or not the test case has change
      const testCase = TestCase.fromFile(testCaseFile);
      assert.ok(testCase);
      await activatedExtension();
      // Run the test case:
      await testCase.assertSuccess(testCasesDir, testFilesDir);
    });
  });
});

async function activatedExtension(): Promise<Extension<Gilear.ExtensionAPI>> {
  const ext = vscode.extensions.getExtension<Gilear.ExtensionAPI>(
    Gilear.extensionId,
  );
  if (!ext.isActive) await ext.activate();
  return ext;
}
