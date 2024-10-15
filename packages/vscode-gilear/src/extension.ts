import * as path from "path";
import * as vscode from "vscode";
import {
  Executable,
  LanguageClient,
  LanguageClientOptions,
  Logger,
  ServerOptions,
  TransportKind,
} from "vscode-languageclient/node";

export type GilearExtensionAPI = {};

let client: LanguageClient;

export function activate(context: vscode.ExtensionContext): GilearExtensionAPI {
  // Options for running the LSP Server.
  const serverOptions: ServerOptions = {
    run: {
      command: "gilear-lsp",
    },
    debug: {
      command: "cabal",
      args: ["run", "gilear-lsp"],
    },
  };

  // Options for the LSP Client.
  const clientOptions: LanguageClientOptions = {
    // Register the server for Gilear documents.
    documentSelector: [{ scheme: "file", language: "gilear" }],
    synchronize: {
      // Notify the server about changes to .gilear files in the workspace.
      fileEvents: vscode.workspace.createFileSystemWatcher("*.gilear"),
    },
    // Create an output channel for LSP trace messages.
    traceOutputChannel: vscode.window.createOutputChannel(
      "Gilear Log",
      "gilear",
    ),
  };

  // Create a Language Server Client.
  client = new LanguageClient("Gilear", serverOptions, clientOptions);

  // Start the client and launch the server.
  client.start();

  // Return the API provided by the Gilear extension.
  return {};
}

export function deactivate(): Thenable<void> | undefined {
  if (!client) {
    return undefined;
  }
  return client.stop();
}
