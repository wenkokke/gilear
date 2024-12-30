#!/bin/sh

# Build the test binary optimized and with debug symbols
cabal build tree-sitter-test --ghc-options="-g -O2"

# Find the test binary
tree_sitter_test=$(echo "dist-newstyle/build/"*"/ghc-"*"/tree-sitter-"*"/t/tree-sitter-test/build/tree-sitter-test/tree-sitter-test")

# Run the test binary with -p '/test_parseJQuery/'
tree_sitter_datadir="packages/tree-sitter" ${tree_sitter_test} -p '/test_parseJQuery/'
