import * as path from "path";
import { runTests } from "@vscode/test-electron";

async function main() {
  const extensionDevelopmentPath = path.resolve(
    __dirname,
    path.join("..", "..", "..", ".."),
  );
  const extensionTestsPath = path.resolve(__dirname, "suite");
  await runTests({
    extensionDevelopmentPath,
    extensionTestsPath,
  });
}
main();
