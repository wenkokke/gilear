import * as assert from "assert";
import * as vscode from "vscode";
import * as fs from "fs";
import * as path from "path";

export class GoldenTest {
  static fileExt = ".test.json";

  readonly name: string;
  readonly file: string;
  readonly steps: Step[];
  diagnosis: Promise<any>;

  constructor(name: string, file: string, steps: Step[]) {
    this.name = name;
    this.file = file;
    this.steps = steps;
  }

  static fromFile(goldenTestFile: string): GoldenTest {
    const name = path.basename(goldenTestFile, GoldenTest.fileExt);
    const goldenTestContent = fs.readFileSync(goldenTestFile, {
      encoding: "utf-8",
    });
    const goldenTestInfo = JSON.parse(goldenTestContent);
    assert(typeof goldenTestInfo?.file === "string");
    assert(Array.isArray(goldenTestInfo?.steps));
    return new GoldenTest(name, goldenTestInfo.file, goldenTestInfo.steps);
  }

  toFile(goldenTestDir: string): void {
    const goldenTestFile = path.join(
      goldenTestDir,
      `${this.name}${GoldenTest.fileExt}`,
    );
    const goldenTestContents = JSON.stringify({
      file: this.file,
      steps: this.steps,
    });
    fs.writeFileSync(goldenTestFile, goldenTestContents);
  }

  triggerDiagnosis(step, editor) {
    vscode.languages.onDidChangeDiagnostics(
      (event: vscode.DiagnosticChangeEvent) => {
        this.diagnosis = new Promise((resolve, reject) => {
          if (event.uris.toString() == editor.document.uri.toString()) {
            const expect = step.diagnostics;
            const actual = vscode.languages
              .getDiagnostics(editor.document.uri)
              .map(toDiagnostic);

            try {
              assert.deepStrictEqual(actual, expect);
              resolve(actual);
            } catch (e) {
              reject(e);
              throw e;
            }
          }
        });
      },
      this,
    );
  }

  async assertSuccess(
    goldenTestCasesDir: string,
    goldenTestFilesDir: string,
  ): Promise<void> {
    // Validate the golden test...
    // Track whether or not the test case is being updated...
    const shouldUpdate = this.shouldUpdateGolden();
    // Track whether or not the test case has any changes...
    let hasChanges = false;
    // Maintain an updated golden test...
    const updatedGoldenTest = new GoldenTest(this.name, this.file, []);
    // Open the golden test's associated file...
    const docFile = path.join(goldenTestFilesDir, this.file);
    const doc = await vscode.workspace.openTextDocument(docFile);
    const editor = await vscode.window.showTextDocument(doc);

    try {
      for (const step of this.steps) {
        // Apply the edits...
        await editor.edit((editorEdit) => {
          this.triggerDiagnosis(step, editor);
          for (const edit of step.edits) {
            editorEdit.replace(fromRange(edit.range), edit.text);
          }
        });
        // TODO: more user friendly error reporting on diagnostics
        //       1. check if only the order differs (i.e. compare as sets)
        //       2. remove the diagnostics that are in both sets
        //       3. visualise the diagnostics that are in one set but not the other
        await sleep(150);
        const actual = await this.diagnosis;
        const expect = step.diagnostics;

        try {
          assert.deepStrictEqual(actual, expect);
        } catch (e) {
          throw e;
        }

        updatedGoldenTest.steps.push({
          edits: step.edits,
          diagnostics: actual,
        });

        // If we should update golden tests, do so...
        if (shouldUpdate && hasChanges) {
          updatedGoldenTest.toFile(goldenTestCasesDir);
        }
      }
    } catch (e) {
      throw e;
    } finally {
      // Close the file without saving...
      await vscode.commands.executeCommand(
        "workbench.action.revertAndCloseActiveEditor",
      );
    }
  }

  shouldUpdateGolden(): boolean {
    return (
      process.env.TEST_UPDATE === "true" ||
      process.env.npm_config_test_update === "true"
    );
  }
}

async function sleep(ms: number): Promise<void> {
  return await new Promise((resolve) => setTimeout(resolve, ms));
}

export interface Step {
  edits: Edit[];
  diagnostics: Diagnostic[];
}

export function assertIsStep(step: any): step is Step {
  try {
    assertIsEdits(step);
    return true;
  } catch (e) {
    // TODO: combine assertion failures for better error reporting
    assertIsDiagnostics(step);
    return true;
  }
}

export interface Edits {
  edits: Edit[];
}

export function assertIsEdits(edits: any): edits is Edits {
  assert(Array.isArray(edits?.edits));
  return edits.edits.every(assertIsEdit);
}

export interface Edit {
  range: Range;
  text: string;
}

export function toEdit(edit: vscode.TextDocumentContentChangeEvent): Edit {
  const { range, text } = edit;
  return { range: toRange(range), text };
}

export function assertIsEdit(edit: any): edit is Edit {
  assertIsRange(edit?.range);
  assert(typeof edit?.text === "string");
  return true;
}

export interface Range {
  start: Position;
  end: Position;
}

export function toRange(range: vscode.Range): Range {
  const { start, end } = range;
  return { start: toPosition(start), end: toPosition(end) };
}

export function fromRange(range: Range): vscode.Range {
  return new vscode.Range(fromPosition(range.start), fromPosition(range.end));
}

export function assertIsRange(range: any): range is Range {
  assertIsPosition(range?.start);
  assertIsPosition(range?.end);
  return true;
}

export interface Position {
  line: number;
  character: number;
}

export function toPosition(position: vscode.Position): Position {
  const { character, line } = position;
  return { character, line };
}

export function fromPosition(position: Position): vscode.Position {
  return new vscode.Position(position.line, position.character);
}

export function assertIsPosition(position: any): position is Position {
  assert(typeof position?.line === "number");
  assert(typeof position?.character === "number");
  return true;
}

export type Severity = "error" | "warning" | "information" | "hint";

export function toSeverity(severity: vscode.DiagnosticSeverity): Severity {
  switch (severity) {
    case vscode.DiagnosticSeverity.Error:
      return "error";
    case vscode.DiagnosticSeverity.Warning:
      return "warning";
    case vscode.DiagnosticSeverity.Information:
      return "information";
    case vscode.DiagnosticSeverity.Hint:
      return "hint";
  }
}

export function assertIsSeverity(severity: any): severity is Severity {
  assert(["error", "warning", "information", "hint"].includes(severity));
  return true;
}

export interface Diagnostics {
  diagnostics: Diagnostic[];
}

export function assertIsDiagnostics(
  diagnostics: any,
): diagnostics is Diagnostics {
  assert(Array.isArray(diagnostics?.diagnostics));
  return diagnostics.diagnostics.every(assertIsDiagnostic);
}

export interface Diagnostic {
  severity: Severity;
  range: Range;
  message: string;
}

export function toDiagnostic(diagnostic: vscode.Diagnostic): Diagnostic {
  const { severity, range, message } = diagnostic;
  return { severity: toSeverity(severity), range: toRange(range), message };
}

export function assertIsDiagnostic(diagnostic: any): diagnostic is Diagnostic {
  assertIsSeverity(diagnostic?.severity);
  assertIsRange(diagnostic?.range);
  assert(typeof diagnostic?.message === "string");
  return true;
}
