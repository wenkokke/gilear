import { globSync } from "glob";
import * as assert from "assert";
import * as path from "path";
import * as vscode from "vscode";
import * as Gilear from "../extension";
import { Extension } from "vscode";
import { fromRange, TestCase, toDiagnostic } from "./TestCase";

const testDir = path.join(__dirname, "..", "..", "src", "test");
const testCasesDir = path.join(testDir, "tests");
const testFilesDir = path.join(testCasesDir, "files");
const testCasePattern = path.join(testCasesDir, `*${TestCase.fileExt}`);
const testCaseOptions = { windowsPathsNoEscape: true };

async function activatedExtension(): Promise<Extension<Gilear.ExtensionAPI>> {
  const extId = "wenkokke.vscode-gilear";
  const ext = vscode.extensions.getExtension<Gilear.ExtensionAPI>(extId);
  if (!ext.isActive) await ext.activate();
  return ext;
}

async function sleep(ms: number): Promise<void> {
  return await new Promise((resolve) => setTimeout(resolve, ms));
}

// TODO: run cabal build gilear-lsp before starting tests
suite("Extension Test Suite", () => {
  suiteSetup(async () => {
    vscode.window.showInformationMessage("Start all tests.");
  });

  test("Is vscode-gilear available?", async () => {
    const ext = await activatedExtension();
    assert.ok(
      ext,
      "VS Code extension `wenkokke.vscode-gilear` is not available",
    );
    assert(
      ext.exports.client.isRunning(),
      "VS Code extension `wenkokke.vscode-gilear` is not running",
    );
  });

  const testCaseFiles = globSync(testCasePattern, testCaseOptions);

  test("Are there any tests?", () => {
    assert(
      testCaseFiles.length > 0,
      `The glob pattern ${testCasePattern} found no tests`,
    );
  });

  testCaseFiles.sort().forEach((testCaseFile) => {
    const name = path.basename(testCaseFile, TestCase.fileExt);
    const title = `Test: ${name}`;
    test(title, async () => {
      const testCase = TestCase.fromFile(testCaseFile);
      assert.ok(testCase);
      await activatedExtension();
      const docFile = path.join(testFilesDir, testCase.file);
      const doc = await vscode.workspace.openTextDocument(docFile);
      const editor = await vscode.window.showTextDocument(doc);
      for (const step of testCase.test) {
        // Sleep to allow the LSP to process...
        await sleep(100);
        if ("edits" in step) {
          // If the step is an edit, apply it:
          await editor.edit((editorEdit) => {
            for (const edit of step.edits) {
              editorEdit.replace(fromRange(edit.range), edit.text);
            }
          });
        } else {
          // If the step is a diagnostic, verify it:
          const expected = step.diagnostics;
          const actual = vscode.languages.getDiagnostics(editor.document.uri);
          assert.equal(actual.length, expected.length);
          for (let index = 0; index < expected.length; index += 1) {
            assert.deepStrictEqual(
              toDiagnostic(actual[index]),
              expected[index],
            );
          }
        }
      }
    });
  });
});
