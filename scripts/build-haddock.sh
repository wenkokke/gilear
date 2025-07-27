#!/bin/sh -e

# Read the expected version:
EXPECT_VERSION="$(awk -F'=' '/^haddock=/{print$2}' ./scripts/dev-dependencies.txt)"

# Find haddock:
#
# 1. Use HADDOCK if it is set.
# 2. Look for haddock-$EXPECTED_VERSION.
# 3. Look for haddock.
#
if [ "${HADDOCK}" = "" ]; then
  if ! HADDOCK="$(which "haddock-${EXPECT_VERSION}")"; then
    if ! HADDOCK="$(which "haddock")"; then
      echo "Requires haddock ${EXPECT_VERSION}; no version found"
      echo "To install, run:"
      echo
      echo "  cabal install haddock-${EXPECT_VERSION}"
      echo
      exit 1
    fi
  fi
fi

# Check haddock version:
ACTUAL_VERSION="$("${HADDOCK}" --version | head -n 1 | cut -d' ' -f3 | sed -E 's/(.*),/\1/')"
if [ "${ACTUAL_VERSION}" != "${EXPECT_VERSION}" ]; then
  echo "Requires haddock ${EXPECT_VERSION}; version ${ACTUAL_VERSION} found"
  # Version mismatch is an error on CI:
  [ "${CI}" = "" ] || exit 1
fi

# Build the Haddock documentation for all local packages.
# NOTE(cabal.project): This relies on the configuration under 'Documentation' in cabal.project.
cabal haddock all

# Create the './doc/reference' directory
mkdir -p "./doc/reference/"

# Copy the Haddock documentation for each local package to the './doc/reference' directory
find "./dist-newstyle/build" -type d -and -name "html" -exec cp -r {} ./doc/reference/ ';'

# Build the Haddock documentation index
# NOTE(newpackage): This command must have one --read-interface argument for each package.
${HADDOCK} \
  -o ./doc/reference \
  --quickjump \
  --gen-index \
  --gen-contents \
  --read-interface=data-debruijn,./doc/reference/data-debruijn/data-debruijn.haddock \
  --read-interface=gilear,./doc/reference/gilear/gilear.haddock \
  --read-interface=gilear-lsp,./doc/reference/gilear-lsp/gilear-lsp.haddock \
  --read-interface=tree-sitter-gilear,./doc/reference/tree-sitter-gilear/tree-sitter-gilear.haddock
