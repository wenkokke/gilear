# Scripts

The scripts in this directory are intended to automate various common tasks. Every script in this directory must be written a POSIX Shell Script and must start with `#!/bin/sh`. This is checked by `lint-shellcheck.sh`. The reason for strict POSIX Shell compliance is that the POSIX Shell is incredibly portable. Any system that runs Git runs a POSIX shell. This is not true for other shells such as BASH and ZSH.

The dependencies of these scripts are listed in `dev-dependencies.txt` using a simple format where each line lists the name of one executable and its version separated by an equals sign.

## Git Hooks

The `pre-commit.sh` script is indended to run as a Git pre-commit hook and run every other linting and formatting script in the directory. To install it as a Git pre-commit hook, symlink it into your `.git/hooks` directory:

```sh
ln scripts/pre-commit.sh .git/hooks/pre-commit.sh
```

## Code Quality

The `format-X.sh` and `lint-X.sh` scripts in this directory are there to ensure code consistency and check for code quality. These are named by concatenating their primary action (either _format_ or _lint_) and the _single_ tool that it runs. For instance, `format-fourmolu.sh` _formats_ the Haskell code in the repository using the _fourmolu_ executable.

## TODO

- Add some sort of formatter for the Nix configuration. This cannot be `nix fmt` as this would require that every contributor has Nix installed. The [Alejandra](https://github.com/kamadorueda/alejandra) formatter is an option, but this would add a dependency on a Rust package.
- Add some sort of formatter for POSIX Shell scripts.
- Modify `lint-shellcheck.sh` to ensure that the scripts in this directory are not only correct shell scripts, but that they are POSIX Shell scripts.
- Modify all scripts to ensure that they can safely be run from any subdirectory of this repository. Presently, scripts can only be run from the repository root. (This [StackOverflow answer](https://stackoverflow.com/a/29835459) has a solution that has worked well for Wen in the past.)
