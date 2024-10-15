# Style Guide

This file provides style guidelines for contributing to the Gilear codebase.

## Packages

### Haskell

#### Library Names and Exposed Modules

Each library should expose a single module whose name matches the library name. For instance, the library `gilear` exposes the module `Gilear` and the library `gilear-lsp` exposes the module `Gilear.LSP`. Any other module should be under the `Internal` module path, e.g., `Gilear.LSP.Internal.Core`. Internal modules may be exposed, for use in testing, but should never be imported by other packages.

#### Optional Features and Sub-Libraries

Cabal packages must expose the same API regardless of the build flags. Optional features must be provided via sub-libraries. Consider that we might want to make the Gilear AST an instance of the `NoThunks` class, but we do not wish to saddle every project with a runtime dependency on `nothunks`. To solve this, we would make a sub-library of the `gilear` package, named `gilear-nothunks`, which adds the dependency on `nothunks` and derives the `NoThunks` instances. In such cases, orphan instances are permitted.

#### Test Suite and Module Names

The test suite associated with a Cabal library `x-y-z` should be named `x-y-z-test`. The tests for the functions defined in module `X.Y.Z` should be defined in the module `Test.X.Y.Z`.
