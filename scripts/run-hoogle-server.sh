#!/bin/sh -e

# Read the expected version:
EXPECT_VERSION="$(awk -F'=' '/^cabal-hoogle=/{print$2}' ./scripts/dev-dependencies.txt)"

# Find cabal-hoogle:
#
# 1. Use CABAL_HOOGLE if it is set.
# 2. Look for cabal-hoogle-$EXPECTED_VERSION.
# 3. Look for cabal-hoogle.
#
if [ "${CABAL_HOOGLE}" = "" ]; then
  if ! CABAL_HOOGLE="$(which "cabal-hoogle-${EXPECT_VERSION}")"; then
    if ! CABAL_HOOGLE="$(which "cabal-hoogle")"; then
      echo "Requires cabal-hoogle ${EXPECT_VERSION}; no version found"
      echo "To install, run:"
      echo
      echo "  cabal install cabal-hoogle-${EXPECT_VERSION}"
      echo
      exit 1
    fi
  fi
fi

# Check cabal-hoogle version:
ACTUAL_VERSION="$("${CABAL_HOOGLE}" --version | head -n 1 | cut -d' ' -f3)"
if [ "${ACTUAL_VERSION}" != "${EXPECT_VERSION}" ]; then
  echo "Requires cabal-hoogle ${EXPECT_VERSION}; version ${ACTUAL_VERSION} found"
  # Version mismatch is an error on CI:
  [ "${CI}" = "" ] || exit 1
fi

# Serve Hoogle
echo "Serve Hoogle with cabal-hoogle version ${ACTUAL_VERSION}"
${CABAL_HOOGLE} run -- server --local --port 9000
