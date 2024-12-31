import { globSync } from "glob";
import * as path from "path";
import * as fs from "fs";
import * as vscode from "vscode";
import * as which from "which";
import {
  Executable,
  LanguageClient,
  LanguageClientOptions,
  ServerOptions,
} from "vscode-languageclient/node";

export type GilearExtensionAPI = {};

let client: LanguageClient;

export function activate(context: vscode.ExtensionContext): GilearExtensionAPI {
  // Create log channel.
  const clientLog = vscode.window.createOutputChannel(
    "Gilear Client",
    "gilear",
  );

  // Create filesystem watcher for the LSP Client.
  const clientFileSystemWatcher =
    vscode.workspace.createFileSystemWatcher("*.gilear");

  // Create options for the LSP Client.
  const clientOptions: LanguageClientOptions = {
    // Register the server for Gilear documents.
    documentSelector: [{ scheme: "file", language: "gilear" }],
    // Notify the server about changes to Gilear files in the workspace.
    synchronize: { fileEvents: clientFileSystemWatcher },
    // Create an output channel for LSP trace messages.
    traceOutputChannel: clientLog,
  };

  // Create options for running the LSP Server.
  const serverOptions: ServerOptions = {
    run: { command: "gilear-lsp" },
    debug: findDebugExecutable(context, clientLog),
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

/** Find executable for `gilear-lsp` when running in Development Mode.
 *
 * This fails if there are multiple versions of `gilear-lsp` under the
 * `dist-newstyle` directory, which may happen when `gilear-lsp` has been
 * compiled by different versions of GHC or for different target systems.
 */
// NOTE(directory-structure):
// This function depends on the name and location of the `vscode-gilear` and
// `gilear-lsp` packages as well as the structure of Cabal's dist-newstyle
// directory and must be updated if those change.
// TODO:
// This function could be refined to search only those directories in the
// `dist-newstyle` directory that match the current platform---which would
// require mapping the names for platforms and archirectures from JavaScript
// (`process.platform` and `process.arch`) to Haskell (`System.Info.os` and
// `System.Info.arch`)---and could choose to prefer the `gilear-lsp` binary
// compiled by the GHC version that is currently on the path, if any.
function findDebugExecutable(
  context: vscode.ExtensionContext,
  logChannel: vscode.OutputChannel,
): Executable | null {
  if (context.extensionMode == vscode.ExtensionMode.Development) {
    // Notify the developer that the extension has started in Development Mode.
    const extensionName = context.extension.packageJSON.displayName;
    log(
      logChannel,
      vscode.LogLevel.Info,
      `${extensionName} started in Development Mode`,
    );
    // Check if the extension is running in a development environment,
    // before we start searching for a `dist-newstyle` directory.
    if (isDevelopmentEnvironment(context)) {
      // Find the executable for `gilear-lsp` in the `dist-newstyle` directory.
      const projectRoot = path.join(
        context.extensionPath,
        "..", // vscode-gilear
        "..", // packages
      );
      const candidates = globSync(
        path.join(
          projectRoot,
          "dist-newstyle",
          "build",
          "*-*",
          "ghc-*",
          "gilear-lsp-*",
          "x",
          "gilear-lsp",
          "build",
          "gilear-lsp",
          "gilear-lsp",
        ),
      );
      // Found exactly one executable for `gilear-lsp`.
      if (candidates.length == 1) {
        const executable = candidates[0];
        const executableRelative = path.dirname(
          path.relative(projectRoot, executable),
        );
        const message = `Found gilear-lsp in '${executableRelative}'`;
        log(logChannel, vscode.LogLevel.Debug, message);
        return { command: executable };
      }
      // Found multiple executables for `gilear-lsp`.
      if (candidates.length >= 2) {
        const candidatesRelative = candidates.map((candidate) =>
          path.dirname(path.relative(projectRoot, candidate)),
        );
        log(
          logChannel,
          vscode.LogLevel.Warning,
          [
            // Log message
            `${extensionName} found multiple candidates for gilear-lsp:`,
            ...candidatesRelative,
          ].join("\n"),
          [
            // User message
            `${extensionName} found multiple candidates for gilear-lsp.`,
            `For details, see output channel '${logChannel.name}'.`,
          ].join("\n"),
        );
      }
      // Could not find `gilear-lsp`.
      if (candidates.length == 0) {
        log(
          logChannel,
          vscode.LogLevel.Warning,
          [
            // Log message
            `${extensionName} could not find gilear-lsp in dist-newstyle.`,
          ].join("\n"),
          [
            // User message
            `${extensionName} could not find gilear-lsp in dist-newstyle.`,
            "Did you forget to run `cabal build gilear-lsp`?",
          ].join("\n"),
        );
      }
      // Default to starting gilear-lsp using Cabal.
      log(
        logChannel,
        vscode.LogLevel.Warning,
        [
          // Log message
          `${extensionName} starting gilear-lsp using Cabal, which may timeout`,
        ].join("\n"),
        [
          // User message
          `${extensionName} starting gilear-lsp using Cabal, which may timeout`,
          "To avoid a timeout, build gilear-lsp before starting the extension.",
        ].join("\n"),
      );
      return { command: "cabal", args: ["run", "gilear-lsp"] };
    }
    // Try and find `gilear-lsp` on the PATH.
    const executable = which.sync("gilear-lsp", { nothrow: true });
    if (executable !== null) {
      log(
        logChannel,
        vscode.LogLevel.Debug,
        [
          // Log message
          `Found gilear-lsp executable on path:`,
          executable,
        ].join("\n"),
      );
      return { command: executable };
    }
    // Raise an error:
    log(
      logChannel,
      vscode.LogLevel.Error,
      [
        // Log and user message
        `${extensionName} cannot start language server, since it could not find gilear-lsp on the PATH and could not recognise its development repository.`,
      ].join("\n"),
    );
    return null;
  }
  return null;
}

/** Check if the extension is being executed from a development environment.
 *
 *  This checks for `cabal` on the path and for the following files:
 *
 *  - `/cabal.project`
 *  - `/packages/gilear-lsp/gilear-lsp.cabal`
 */
// NOTE(directory-structure):
// This function depends on the name and location of the `vscode-gilear` and
// `gilear-lsp` packages and must be updated if those change.
function isDevelopmentEnvironment(context: vscode.ExtensionContext): boolean {
  // Check for `cabal` on the path:
  const cabal = which.sync("cabal", { nothrow: true });
  if (cabal === null) {
    return false;
  }
  // Check for basic configuration files:
  const projectRoot = path.join(
    context.extensionPath,
    "..", // vscode-gilear
    "..", // packages
  );
  const cabalProject = path.join(projectRoot, "cabal.project");
  if (!fs.existsSync(cabalProject)) {
    return false;
  }
  const gilearLspCabal = path.join(
    projectRoot,
    "packages",
    "gilear-lsp",
    "gilear-lsp.cabal",
  );
  if (!fs.existsSync(gilearLspCabal)) {
    return false;
  }
  return true;
}

function log(
  logChannel: vscode.OutputChannel,
  logLevel: vscode.LogLevel,
  message: string,
  userMessage?: string,
) {
  // Append message to the log channel:
  const logMessage = logFormatMessage(logLevel, message);
  logChannel.appendLine(logMessage);
  // Show message to the user:
  userMessage = userMessage ?? message;
  switch (logLevel) {
    case vscode.LogLevel.Info:
      vscode.window.showInformationMessage(userMessage);
      break;
    case vscode.LogLevel.Warning:
      vscode.window.showWarningMessage(userMessage);
      break;
    case vscode.LogLevel.Error:
      vscode.window.showErrorMessage(userMessage);
      break;
  }
}

function logFormatMessage(
  logLevel: vscode.LogLevel,
  logMessage: string,
): string {
  const currentTime = logGetCurrentTime();
  switch (logLevel) {
    case vscode.LogLevel.Info:
      return `[Info  - ${currentTime}] ${logMessage}`;
      break;
    case vscode.LogLevel.Warning:
      return `[Warn  - ${currentTime}] ${logMessage}`;
      break;
    case vscode.LogLevel.Error:
      return `[Error - ${currentTime}] ${logMessage}`;
      break;
    case vscode.LogLevel.Debug:
      return `[Debug - ${currentTime}] ${logMessage}`;
    case vscode.LogLevel.Trace:
      return `[Trace - ${currentTime}] ${logMessage}`;
  }
}

function logGetCurrentTime(): string {
  const now = new Date();
  const nowHours = now.getHours();
  const nowH = (nowHours > 12 ? nowHours - 12 : nowHours).toString();
  const nowMM = now.getMinutes().toString().padStart(2, "0");
  const nowss = now.getSeconds().toString().padStart(2, "0");
  const nowXM = nowHours > 12 ? "PM" : "AM";
  return `${nowH}:${nowMM}:${nowss} ${nowXM}`;
}
