import { globSync } from "glob";
import * as assert from "assert";
import * as path from "path";
import * as vscode from "vscode";
import * as Gilear from "../extension";
import { GoldenTest } from "./GoldenTest";
import * as Mocha from "mocha";

// NOTE(directory-structure): this code depends on the directory that contains the test data
const testDir = path.join(__dirname, "..", "..", "src", "test");
const goldenTestCasesDir = path.join(testDir, "golden");
const goldenTestFilesDir = path.join(goldenTestCasesDir, "files");
const goldenTestCasePattern = path.join(
  goldenTestCasesDir,
  `*${GoldenTest.fileExt}`,
);
const goldenTestCaseOptions = { windowsPathsNoEscape: true };

// NOTE: the golden tests are updated if either the environment variable
//       TEST_UPDATE or the npm configuration option test-update is set
//       to the literal string "true"
const shouldUpdate: boolean =
  process.env.TEST_UPDATE === "true" ||
  process.env.npm_config_test_update === "true";

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

  test("Do the golden tests actually test something?", async () => {
    assert.rejects(async () => {
      const testCase = GoldenTest.fromFile(goldenTestCaseFiles[0]);
      testCase.steps.map((step) => {
        step.diagnostics = [];
      });
      await testCase.run(goldenTestFilesDir);
    });
  });

  test("Does updating the golden tests actually work?", async () => {
    // Load the first test golden test:
    const testCase = GoldenTest.fromFile(goldenTestCaseFiles[0]);
    // Create a broken version of the golden test:
    const brokenTestCase = new GoldenTest(
      testCase.name,
      testCase.file,
      testCase.steps.map((step) => ({ edits: step.edits, diagnostics: [] })),
    );
    // Ensure that the original and broken test cases are not equal:
    assert.notDeepStrictEqual(testCase, brokenTestCase);
    // Run the golden test with shouldUpdate
    const updatedGoldenTest = await brokenTestCase.run(goldenTestFilesDir, {
      shouldUpdate: true,
    });
    // Assert that the original and updated test cases are equal:
    assert.deepStrictEqual(testCase, updatedGoldenTest);
  });

  goldenTestCaseFiles.sort().forEach((testCaseFile) => {
    const name = path.basename(testCaseFile, GoldenTest.fileExt);
    const title = shouldUpdate ? `Update: ${name}` : `Test: ${name}`;
    test(title, async () => {
      // Track whether or not the test case has change
      const testCase = GoldenTest.fromFile(testCaseFile);
      assert.ok(testCase);
      await Gilear.extensionAPI();
      // Run the test case:
      const updatedTestCase = await testCase.run(goldenTestFilesDir, {
        shouldUpdate,
      });
      if (shouldUpdate && updatedTestCase !== null) {
        updatedTestCase.writeTo(goldenTestCasesDir);
      }
    });
  });
});
