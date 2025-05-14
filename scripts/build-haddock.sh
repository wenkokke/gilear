#!/bin/sh

# Check for Haddock
HADDOCK_EXPECT_VERSION=$(awk -F'=' '/^haddock=/{print$2}' ./scripts/dev-dependencies.txt)
if [ "${HADDOCK}" = "" ]; then
    HADDOCK=$(which "haddock")
    if [ "${HADDOCK}" = "" ]; then
        echo "Requires haddock ${HADDOCK_EXPECT_VERSION}; no version found"
        exit 1
    fi
fi
HADDOCK_VERSION_ACTUAL=$(${HADDOCK} --version | head -n 1 | cut -d' ' -f3 | sed -E 's/(.*),/\1/')
if [ ! "${HADDOCK_VERSION_ACTUAL}" = "${HADDOCK_EXPECT_VERSION}" ]; then
    echo "Requires haddock ${HADDOCK_EXPECT_VERSION}; version ${HADDOCK_VERSION_ACTUAL} found"
    exit 1
fi

# Build the Haddock documentation for all local packages.
# NOTE(cabal.project): This relies on the configuration under 'Documentation' in cabal.project.
if ! cabal haddock all; then
    exit 1
fi

# Create the './doc/reference' directory
if ! mkdir -p "./doc/reference/"; then
    exit 1
fi
# Copy the Haddock documentation for each local package to the './doc/reference' directory
if ! find "./dist-newstyle/build" -type d -and -name "html" -exec cp -r {} ./doc/reference/ ';'; then
    exit 1
fi

# Build the Haddock documentation index
# NOTE(newpackage): This command must have one --read-interface argument for each package.
if ! ${HADDOCK} \
  -o ./doc/reference \
  --quickjump \
  --gen-index \
  --gen-contents \
  --read-interface=base-compat-constptr,./doc/reference/base-compat-constptr/base-compat-constptr.haddock \
  --read-interface=control-indexed,./doc/reference/control-indexed/control-indexed.haddock \
  --read-interface=control-kripke,./doc/reference/control-kripke/control-kripke.haddock \
  --read-interface=data-debruijn,./doc/reference/data-debruijn/data-debruijn.haddock \
  --read-interface=gilear,./doc/reference/gilear/gilear.haddock \
  --read-interface=gilear-lsp,./doc/reference/gilear-lsp/gilear-lsp.haddock \
  --read-interface=tree-sitter,./doc/reference/tree-sitter/tree-sitter.haddock \
  --read-interface=tree-sitter-capi,./doc/reference/tree-sitter-capi/tree-sitter-capi.haddock \
  --read-interface=tree-sitter-gilear,./doc/reference/tree-sitter-gilear/tree-sitter-gilear.haddock \
  --read-interface=tree-sitter-javascript,./doc/reference/tree-sitter-javascript/tree-sitter-javascript.haddock \
  --read-interface=tree-sitter-while,./doc/reference/tree-sitter-while/tree-sitter-while.haddock; then
  exit 1
fi
