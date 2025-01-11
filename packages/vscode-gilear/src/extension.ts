import * as vscode from "vscode";
import * as lsp from "vscode-languageclient/node";
import { findExecutable } from "./extension/findExecutable";
import { ArrayLogger } from "./extension/logger/ArrayLogger";
import { VSCodeOutputChannelLoggerAdapter } from "./extension/logger/VSCodeOutputChannelLoggerAdapter";
import { VSCodeWindowLogger } from "./extension/logger/VSCodeWindowLogger";
import { createTestCaseRecorder } from "./test/TestCaseRecorder";

export type ExtensionAPI = {
  client: lsp.LanguageClient;
};

let client: lsp.LanguageClient;

export function activate(
  context: vscode.ExtensionContext,
): Thenable<ExtensionAPI> {
  // Create output channels.
  const traceOutputChannel = vscode.window.createOutputChannel(
    "Gilear Trace",
    "gilear",
  );
  const outputChannel = vscode.window.createOutputChannel("Gilear", "gilear");

  // Create logger.
  const clientLogger = new ArrayLogger([
    new VSCodeOutputChannelLoggerAdapter(traceOutputChannel),
    VSCodeWindowLogger,
  ]);

  // Create filesystem watcher for the LSP Client.
  const clientFileSystemWatcher =
    vscode.workspace.createFileSystemWatcher("*.gilear");

  // Create options for the LSP Client.
  const clientOptions: lsp.LanguageClientOptions = {
    // Register the server for Gilear documents.
    documentSelector: [{ scheme: "file", language: "gilear" }],
    // Notify the server about changes to Gilear files in the workspace.
    synchronize: { fileEvents: clientFileSystemWatcher },
    // Add an output channel for server output.
    traceOutputChannel,
    // Add an output channel for client output.
    outputChannel: outputChannel,
    middleware: createTestCaseRecorder(context, outputChannel),
  };

  // Find the gilear-lsp executable:
  const gilearLspCommand = findExecutable(clientLogger, context);

  // Create options for running the LSP Server.
  const serverOptions: lsp.ServerOptions = {
    command: gilearLspCommand,
  };

  // Create a Language Server Client.
  client = new lsp.LanguageClient("Gilear", serverOptions, clientOptions);

  // Return the API provided by the Gilear extension.
  return client.start().then(
    (value: void) => {
      return {
        client,
      };
    },
    (reason: any) => {
      throw reason;
    },
  );
}

export function deactivate(): Thenable<void> | undefined {
  if (!client) {
    return undefined;
  }
  return client.stop();
}
