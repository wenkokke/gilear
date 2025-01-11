import * as assert from "assert";
import * as vscode from "vscode";
import * as fs from "fs";

export class TestCase {
  static fileExt = ".test.json";

  static fromFile(testCasePath: string): TestCase {
    const testCaseContent = fs.readFileSync(testCasePath, {
      encoding: "utf-8",
    });
    const testCaseInfo = JSON.parse(testCaseContent);
    assert(typeof testCaseInfo?.name === "string");
    assert(typeof testCaseInfo?.file === "string");
    assert(Array.isArray(testCaseInfo?.test));
    const testCase = new TestCase(testCaseInfo.name, testCaseInfo.file);
    for (const testCaseStep of testCaseInfo.test) {
      assert(assertIsStep(testCaseStep));
      testCase.test.push(testCaseStep);
    }
    return testCase;
  }

  readonly name: string;
  readonly file: string;
  readonly test: Step[];

  constructor(name: string, file: string) {
    this.name = name;
    this.file = file;
    this.test = [];
  }
}

export type Step = Edits | Diagnostics;

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
