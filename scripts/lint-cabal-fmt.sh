#!/bin/sh

# Check for cabal-fmt
CABAL_FMT_EXPECT_VERSION=$(awk -F'=' '/^cabal-fmt=/{print$2}' ./scripts/dev-dependencies.txt)
if [ "${CABAL_FMT}" = "" ]; then
    CABAL_FMT=$(which "cabal-fmt")
    if [ "${CABAL_FMT}" = "" ]; then
        echo "Requires cabal-fmt ${CABAL_FMT_EXPECT_VERSION}; no version found"
        echo "To install, run:"
        echo
        echo "  cabal install cabal-fmt-${CABAL_FMT_EXPECT_VERSION}"
        echo
        exit 1
    fi
fi
CABAL_FMT_ACTUAL_VERSION=$(${CABAL_FMT} --version | head -n 1)
if [ ! "${CABAL_FMT_ACTUAL_VERSION}" = "${CABAL_FMT_EXPECT_VERSION}" ]; then
    echo "Requires cabal-fmt ${CABAL_FMT_EXPECT_VERSION}; version ${CABAL_FMT_ACTUAL_VERSION} found"
    exit 1
fi

# Lint Cabal source
echo "Lint Cabal files..."
# shellcheck disable=SC2086
git ls-files --exclude-standard --no-deleted --deduplicate '*.cabal' | xargs -L1 ${CABAL_FMT} --check
