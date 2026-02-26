const { workspace } = require("vscode");
const { LanguageClient, TransportKind } = require("vscode-languageclient/node");

let client;

function activate(context) {
  // Look for the binary in this order:
  // 1. gaul.lsp.path setting
  // 2. cargo build output in the workspace
  // 3. "gaul-lsp" on PATH
  const config = workspace.getConfiguration("gaul");
  const configPath = config.get("lsp.path");

  let command;
  if (configPath) {
    command = configPath;
  } else {
    // Try the cargo target directory relative to workspace root
    const folders = workspace.workspaceFolders;
    if (folders && folders.length > 0) {
      const root = folders[0].uri.fsPath;
      const cargoDebug = require("path").join(
        root,
        "target",
        "debug",
        "gaul-lsp"
      );
      if (require("fs").existsSync(cargoDebug)) {
        command = cargoDebug;
      }
    }
    if (!command) {
      command = "gaul-lsp";
    }
  }

  const serverOptions = {
    command,
    transport: TransportKind.stdio,
  };

  const clientOptions = {
    documentSelector: [{ scheme: "file", language: "gaul" }],
  };

  client = new LanguageClient(
    "gaul-lsp",
    "Gaul Language Server",
    serverOptions,
    clientOptions
  );

  client.start();
}

function deactivate() {
  if (client) {
    return client.stop();
  }
}

module.exports = { activate, deactivate };
