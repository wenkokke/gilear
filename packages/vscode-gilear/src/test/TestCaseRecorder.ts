import * as crypto from "crypto";
import * as vscode from "vscode";
import * as lsp from "vscode-languageclient";
import { TestCase, toDiagnostic, toEdit } from "./TestCase";
import * as path from "path";
import * as fs from "fs";
import { globSync } from "glob";
import * as assert from "assert";

export const commandIdStartRecordingTestCase = "gilear.startTestCaseRecorder";

export const commandIdStopRecordingTestCase = "gilear.stopTestCaseRecorder";

export function createTestCaseRecorder(
  context: vscode.ExtensionContext,
  outputChannel: vscode.OutputChannel,
): TestCaseRecorder | undefined {
  if (context.extensionMode == vscode.ExtensionMode.Development) {
    return new TestCaseRecorder(context, outputChannel);
  }
}

export class TestCaseRecorder implements lsp.Middleware {
  ongoing?: {
    uri: vscode.Uri;
    fileIsNew: boolean;
    test: TestCase;
  };

  readonly testCasesDir: string;
  readonly testFilesDir: string;
  readonly outputChannel: vscode.OutputChannel;

  constructor(
    context: vscode.ExtensionContext,
    outputChannel: vscode.OutputChannel,
  ) {
    // NOTE(testPath): this code depends on the directory that contains the test data
    this.testCasesDir = path.join(
      context.extensionPath,
      "src",
      "test",
      "tests",
    );
    this.testFilesDir = path.join(this.testCasesDir, "files");
    this.outputChannel = outputChannel;
    // Register Gilear: Start Recording Test Case
    context.subscriptions.push(
      vscode.commands.registerCommand(
        commandIdStartRecordingTestCase,
        async () => await this.startRecording(),
      ),
    );
    // Register Gilear: Stop Recording Test Case
    context.subscriptions.push(
      vscode.commands.registerCommand(
        commandIdStopRecordingTestCase,
        async () => await this.stopRecording(),
      ),
    );
  }

  async didChange(
    data: vscode.TextDocumentChangeEvent,
    next: (data: vscode.TextDocumentChangeEvent) => Promise<void>,
  ): Promise<void> {
    if (this && this.isOngoing(data.document.uri)) {
      this.ongoing.test.test.push({
        edits: data.contentChanges.map(toEdit),
      });
    }
    return next(data);
  }

  handleDiagnostics(
    uri: vscode.Uri,
    diagnostics: vscode.Diagnostic[],
    next: lsp.HandleDiagnosticsSignature,
  ): void {
    if (this && this.isOngoing(uri)) {
      this.ongoing.test.test.push({
        diagnostics: diagnostics.map(toDiagnostic),
      });
    }
    return next(uri, diagnostics);
  }

  /**
   * Start recording a test case.
   */
  async startRecording(): Promise<void> {
    if (
      vscode.window.activeTextEditor === undefined ||
      vscode.window.activeTextEditor.document.languageId !== "gilear"
    ) {
      vscode.window.showErrorMessage(
        `Cannot record test: The active document must be a Gilear file.`,
      );
      return;
    }
    // If there is an ongoing test recording, stop it:
    if (this.ongoing !== null) this.stopRecording();
    try {
      // Get the active document:
      const document = vscode.window.activeTextEditor.document;
      // Copy the active document to the test files directory:
      const { file, fileIsNew } = this.writeTestFile(
        document.fileName,
        document.getText(),
      );
      // Get the test case name:
      const namePlaceHolder = this.suggestTestName(file);
      const nameUserInput = await vscode.window.showInputBox({
        title: "Test Name",
        placeHolder: namePlaceHolder,
      });
      if (nameUserInput === undefined) return; // The user cancelled the recording.
      const name = nameUserInput !== "" ? nameUserInput : namePlaceHolder;
      // Create an empty test case:
      this.ongoing = {
        uri: document.uri,
        fileIsNew,
        test: new TestCase(name, file),
      };
      // Notify the user:
      const baseName = path.basename(document.fileName);
      vscode.window.showInformationMessage(`Recording test for ${baseName}`);
    } catch (e) {
      if (e instanceof Error) {
        vscode.window.showErrorMessage(`Cannot record test: ${e.message}`);
      }
    }
  }

  async stopRecording(): Promise<void> {
    if (this && this.ongoing && this.ongoing.test && this.ongoing.test.name) {
      vscode.window
        .showInformationMessage(
          `Save test to '${this.ongoing.test.name}${TestCase.fileExt}'?`,
          "OK",
          "Cancel",
        )
        .then(
          (value: "OK" | "Cancel") => {
            switch (value) {
              case "OK":
                return this.writeTestCaseFile();
              case "Cancel":
                return this.abortOngoing();
            }
          },
          (reason: any) => {
            vscode.window.showErrorMessage(`Could not save test: ${reason}`);
            return this.abortOngoing();
          },
        );
    }
  }

  abortOngoing(): void {
    if (this && this.ongoing) {
      const { test: testCase, fileIsNew } = this.ongoing;
      const { file } = testCase;
      const filePath = path.join(this.testFilesDir, file);
      if (fileIsNew === true && fs.existsSync(filePath)) {
        return fs.rmSync(filePath);
      }
      this.ongoing = null;
    }
  }

  isOngoing(uri: vscode.Uri): boolean {
    return (
      this &&
      this.ongoing &&
      this.ongoing.uri &&
      this.ongoing.uri.toString() === uri.toString()
    );
  }

  /**
   * Suggests a name for the test case, based on the file name of the active document.
   *
   * @returns A suggested name.
   */
  suggestTestName(fileName: string): string {
    // If there is no test case named $basename, suggest $basename:
    const baseName = path.basename(fileName, ".gilear");
    const basePath = path.join(
      this.testCasesDir,
      `${baseName}${TestCase.fileExt}`,
    );
    if (!fs.existsSync(basePath)) {
      return baseName;
    }
    // Otherwise, find the next available index $nextIndex and suggest $basename-$nextIndex:
    const indexedNamePattern = path.join(
      this.testCasesDir,
      `${baseName}-*${TestCase.fileExt}`,
    );
    const indexedNamesInUse = globSync(indexedNamePattern, {
      windowsPathsNoEscape: true,
    });
    const indexesInUse = indexedNamesInUse
      .flatMap((indexedName) => {
        const index = parseInt(
          path
            .basename(indexedName, TestCase.fileExt)
            .substring(baseName.length + 1),
        );
        return isFinite(index) ? [index] : [];
      })
      .sort();
    for (let i = 0; i < indexesInUse.length; i += 1) {
      const nextIndex = i + 1;
      if (nextIndex < indexesInUse[i]) {
        return `${baseName}-${nextIndex}`;
      }
    }
    const nextIndex = indexesInUse.length + 1;
    return `${baseName}-${nextIndex}`;
  }

  /**
   * Copy the file to the test data directory.
   *
   * @param fileName The file name.
   * @param fileContent The file contents.
   * @returns The path to the copy of the file.
   */
  writeTestFile(
    fileName: string,
    fileContent: string,
  ): { file: string; fileIsNew: boolean } {
    const name = path.basename(fileName, ".gilear");
    const hash = Buffer.from(
      crypto.createHash("sha256").update(fileContent).digest("hex"),
    ).toString("base64");
    // Find the shortest prefix of the hash (>=4) that does not result in a hash collision.
    for (let n = 4; n < hash.length; n += 1) {
      const file = `${name}-${hash.substring(0, n)}.gilear`;
      const filePath = path.join(this.testFilesDir, file);
      if (!fs.existsSync(filePath)) {
        fs.writeFileSync(filePath, fileContent);
        return { file: file, fileIsNew: true };
      } else {
        const existingFileContent = fs.readFileSync(filePath, "utf-8");
        const existingHash = Buffer.from(
          crypto.createHash("sha256").update(existingFileContent).digest("hex"),
        ).toString("base64");
        if (hash === existingHash) {
          // The file already exists.
          assert(fileContent === existingFileContent);
          return { file: file, fileIsNew: false };
        } else {
          // Find the first point where hash and existingHash differ and continue.
          assert(hash.substring(0, n) === existingHash.substring(0, n));
          do {
            n += 1;
          } while (hash[n] === existingHash[n]);
          continue;
        }
      }
    }
    // TODO: resolve SHA256 hash collisions with an additional counter.
    throw Error(
      "The active document text has a SHA256 hash collision with an existing test.",
    );
  }

  writeTestCaseFile(): void {
    if (this && this.ongoing) {
      const { name } = this.ongoing.test;
      const testCaseFile = path.join(
        this.testCasesDir,
        `${name}${TestCase.fileExt}`,
      );
      const testCaseContents = JSON.stringify(this.ongoing.test);
      fs.writeFileSync(testCaseFile, testCaseContents);
      this.ongoing = null;
    }
  }
}
