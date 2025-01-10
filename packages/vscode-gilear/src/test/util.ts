import assert = require("assert");
import * as vscode from "vscode";

export async function sleep(ms: number): Promise<void> {
  return await new Promise((resolve) => setTimeout(resolve, ms));
}

export interface TestSpecs {
  tests: TestSpec[];
}

export interface TestSpec {
  file: string;
  edits: EditSpec[];
  diagnostics: DiagnosticSpec[];
}

export type EditSpec =
  | {
      action: "delete";
      location: [[number, number], [number, number]];
    }
  | {
      action: "insert";
      location: [number, number];
      text: string;
    }
  | {
      action: "replace";
      location: [[number, number], [number, number]];
      text: string;
    };

export type DiagnosticSeverity = "error" | "warning" | "information" | "hint";

export interface DiagnosticSpec {
  severity: DiagnosticSeverity;
  range: [[number, number], [number, number]];
  message: string;
}

export type Range = [[number, number], [number, number]];

export type Position = [number, number];

export function isTestSpecs(testSpecs: any): testSpecs is TestSpec[] {
  return Array.isArray(testSpecs) && testSpecs.every(isTestSpec);
}

export function isTestSpec(testSpec: any): testSpec is TestSpec {
  assert(typeof testSpec === "object");
  assert("file" in testSpec);
  assert(typeof testSpec.file === "string");
  assert("edits" in testSpec);
  isEditSpecs(testSpec.edits);
  assert("diagnostics" in testSpec);
  isDiagnosticSpecs(testSpec.diagnostics);
  return true;
}

export function isEditSpecs(editSpecs: any): editSpecs is EditSpec[] {
  assert(Array.isArray(editSpecs));
  editSpecs.every(isEditSpec);
  return true;
}

export function isEditSpec(editSpec: any): editSpec is EditSpec {
  assert(typeof editSpec === "object");
  assert("action" in editSpec);
  assert(typeof "action" === "string");
  switch (editSpec.action) {
    case "delete": {
      assert("location" in editSpec);
      isRange(editSpec.location);
      return true;
    }
    case "insert": {
      assert("location" in editSpec);
      isPosition(editSpec.location);
      assert("text" in editSpec);
      assert(typeof editSpec.text === "string");
      return true;
    }
    case "replace": {
      assert("location" in editSpec);
      isRange(editSpec.location);
      assert("text" in editSpec);
      assert(typeof editSpec.text === "string");
      return true;
    }
    default:
      assert.fail(`unknown action ${editSpec.action}`);
  }
}

export function isDiagnosticSpecs(
  diagnostics: any,
): diagnostics is DiagnosticSpec[] {
  assert(Array.isArray(diagnostics));
  diagnostics.every(isDiagnosticSpec);
  return true;
}

export function isDiagnosticSeverity(
  diagnosticSeverity: any,
): diagnosticSeverity is DiagnosticSeverity {
  assert(typeof diagnosticSeverity === "string");
  assert(
    ["error", "warning", "information", "hint"].includes(diagnosticSeverity),
  );
  return true;
}

export function isDiagnosticSpec(
  diagnostic: any,
): diagnostic is DiagnosticSpec {
  assert(typeof diagnostic === "object");
  assert("severity" in diagnostic);
  isDiagnosticSeverity(diagnostic.severity);
  assert("range" in diagnostic);
  isRange(diagnostic.range);
  assert("message" in diagnostic);
  assert(typeof diagnostic.message === "string");
  return true;
}

export function isRange(range: any): range is Range {
  assert(Array.isArray(range));
  assert(range.length === 2);
  isPosition(range[0]);
  isPosition(range[1]);
  return true;
}

export function isPosition(position: any): position is Position {
  assert(Array.isArray(position));
  assert(position.length === 2);
  assert(typeof position[0] === "number");
  assert(typeof position[1] === "number");
  return true;
}

export function fromDiagnosticSeverity(
  diagnosticSeverity: vscode.DiagnosticSeverity,
): DiagnosticSeverity {
  switch (diagnosticSeverity) {
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

export function fromDiagnostic(diagnostic: vscode.Diagnostic): DiagnosticSpec {
  return {
    severity: fromDiagnosticSeverity(diagnostic.severity),
    range: fromRange(diagnostic.range),
    message: diagnostic.message,
  };
}

export function fromRange(range: vscode.Range): Range {
  return [fromPosition(range.start), fromPosition(range.end)];
}

export function fromPosition(position: vscode.Position): Position {
  return [position.line, position.character];
}

export function toRange(range: Range): vscode.Range {
  return new vscode.Range(toPosition(range[0]), toPosition(range[1]));
}

export function toPosition(position: Position): vscode.Position {
  return new vscode.Position(position[0], position[1]);
}

export function applyEdits(
  editSpecs: EditSpec[],
  edit: vscode.TextEditorEdit,
): void {
  editSpecs.forEach((editSpec) => applyEdit(editSpec, edit));
}

export function applyEdit(
  editSpec: EditSpec,
  edit: vscode.TextEditorEdit,
): void {
  switch (editSpec.action) {
    case "delete":
      edit.delete(toRange(editSpec.location));
      break;
    case "insert":
      edit.insert(toPosition(editSpec.location), editSpec.text);
      break;
    case "replace":
      edit.replace(toRange(editSpec.location), editSpec.text);
      break;
  }
}
