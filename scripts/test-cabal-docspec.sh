#!/bin/sh

# Check for cabal-docspec
CABAL_DOCSPEC_EXPECT_VERSION=$(awk -F'=' '/^cabal-docspec=/{print$2}' ./scripts/dev-dependencies.txt)
if [ "${CABAL_DOCSPEC}" = "" ]; then
    CABAL_DOCSPEC=$(which "cabal-docspec")
    if [ "${CABAL_DOCSPEC}" = "" ]; then
        echo "Requires cabal-docspec ${CABAL_DOCSPEC_EXPECT_VERSION}; no version found"
        echo "To install, see: https://github.com/phadej/cabal-extras/tree/master"
        echo
        exit 1
    fi
fi
CABAL_DOCSPEC_ACTUAL_VERSION=$(${CABAL_DOCSPEC} --version | head -n 1)
if [ ! "${CABAL_DOCSPEC_ACTUAL_VERSION}" = "${CABAL_DOCSPEC_EXPECT_VERSION}" ]; then
    echo "Requires cabal-docspec ${CABAL_DOCSPEC_EXPECT_VERSION}; version ${CABAL_DOCSPEC_ACTUAL_VERSION} found"
    exit 1
fi

# Test Haskell docspec
echo "Test Haskell docspec..."
if [ ! "${CABAL_DOCSPEC_SKIP_BUILD}" = "1" ]; then
    cabal build all || exit 1
fi
"${CABAL_DOCSPEC}" \
    --extra-package=base \
    --extra-package=data-debruijn:arbitrary \
    --extra-package=QuickCheck \
    --check-properties \
    --property-variables="i j m n" \
    --verbose
