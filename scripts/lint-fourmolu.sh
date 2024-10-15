#!/bin/sh

# Check for fourmolu
FOURMOLU_EXPECT_VERSION=$(awk -F'=' '/^fourmolu=/{print$2}' ./scripts/dev-dependencies.txt)
if [ "${FOURMOLU}" = "" ]; then
    FOURMOLU=$(which "fourmolu")
    if [ "${FOURMOLU}" = "" ]; then
        echo "Requires fourmolu ${FOURMOLU_EXPECT_VERSION}; no version found"
        exit 1
    fi
fi
FOURMOLU_ACTUAL_VERSION=$(${FOURMOLU} --version | head -n 1 | cut -d' ' -f2)
if [ ! "${FOURMOLU_ACTUAL_VERSION}" = "${FOURMOLU_EXPECT_VERSION}" ]; then
    echo "Requires fourmolu ${FOURMOLU_EXPECT_VERSION}; version ${FOURMOLU_ACTUAL_VERSION} found"
    exit 1
fi

# Lint Haskell source
echo "Lint Haskell files..."
# shellcheck disable=SC2086
git ls-files --exclude-standard --no-deleted --deduplicate '*.hs' | xargs -L50 ${FOURMOLU} --mode=check
