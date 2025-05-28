#!/bin/sh -e

# Read the expected version:
EXPECT_VERSION="$(awk -F'=' '/^cabal-docspec=/{print$2}' ./scripts/dev-dependencies.txt)"

# Find cabal-docspec:
#
# 1. Use CABAL_DOCSPEC if it is set.
# 2. Look for cabal-docspec-$EXPECTED_VERSION.
# 3. Look for cabal-docspec.
#
if [ "${CABAL_DOCSPEC}" = "" ]; then
  if ! CABAL_DOCSPEC="$(which "cabal-docspec-${EXPECT_VERSION}")"; then
    if ! CABAL_DOCSPEC="$(which "cabal-docspec")"; then
      echo "Requires cabal-docspec ${EXPECT_VERSION}; no version found"
      exit 1
    fi
  fi
fi

# Check cabal-docspec version:
ACTUAL_VERSION="$("${CABAL_DOCSPEC}" --version | head -n 1 | cut -d' ' -f3)"
if [ "${ACTUAL_VERSION}" != "${EXPECT_VERSION}" ]; then
  # Version mismatch is never an error:
  echo "Requires cabal-docspec ${EXPECT_VERSION}; version ${ACTUAL_VERSION} found"
fi

# Test Haskell docs
echo "Test Haskell docs with cabal-docspec version ${ACTUAL_VERSION}"
if [ ! "${CABAL_DOCSPEC_SKIP_BUILD}" = "1" ]; then
  cabal build all || exit 1
fi
"${CABAL_DOCSPEC}" \
  --extra-package=base \
  --extra-package=data-debruijn:arbitrary \
  --extra-package=QuickCheck \
  --check-properties \
  --property-variables="m n" \
  --verbose
