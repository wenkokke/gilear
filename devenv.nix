{ pkgs, lib, config, inputs, ... }:

{

  # TODO: Read dev-dependency versions from ./scripts/dev-dependencies.txt and
  #       pin the corresponding nix dependencies to the appropriate versions.

  # https://devenv.sh/packages/
  packages = [
    pkgs.git
    pkgs.haskellPackages.fourmolu
    pkgs.haskellPackages.cabal-fmt
    pkgs.haskellPackages.ShellCheck
  ];

  # https://devenv.sh/languages/
  languages.haskell = {
    enable = true;
  };
  languages.javascript = {
    enable = true;
  };

  # https://devenv.sh/scripts/
  scripts = {
    pre-commit-hook = {
      description = "Add pre-commit hook to git";
      exec = ''
        echo "Adding pre-commit hook..."
        ln scripts/pre-commit.sh .git/hooks/pre-commit.sh
        '';
    };
  };

  # https://devenv.sh/tests/
  enterTest = ''
    echo "Running tests..."
  '';

}
