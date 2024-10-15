#!/bin/sh

# Check for cabal-hoogle
CABAL_HOOGLE_EXPECT_VERSION=$(awk -F'=' '/^cabal-hoogle=/{print$2}' ./scripts/dev-dependencies.txt)
if [ "${CABAL_HOOGLE}" = "" ]; then
    CABAL_HOOGLE=$(which "cabal-hoogle")
    if [ "${CABAL_HOOGLE}" = "" ]; then
        echo "Requires cabal-hoogle ${CABAL_HOOGLE_EXPECT_VERSION}; no version found"
        exit 1
    fi
fi
CABAL_HOOGLE_VERSION_ACTUAL=$(${CABAL_HOOGLE} --version | head -n 1 | cut -d' ' -f3)
if [ ! "${CABAL_HOOGLE_VERSION_ACTUAL}" = "${CABAL_HOOGLE_EXPECT_VERSION}" ]; then
    echo "Requires cabal-hoogle ${CABAL_HOOGLE_EXPECT_VERSION}; version ${CABAL_HOOGLE_VERSION_ACTUAL} found"
    exit 1
fi

# Create the cabal-hoogle database
${CABAL_HOOGLE} generate
