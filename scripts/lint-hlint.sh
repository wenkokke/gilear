#!/bin/sh

# Check for hlint
HLINT_EXPECT_VERSION=$(awk -F'=' '/^hlint=/{print$2}' ./scripts/dev-dependencies.txt)
if [ "${HLINT}" = "" ]; then
    HLINT=$(which "hlint")
    if [ "${HLINT}" = "" ]; then
        echo "Requires hlint ${HLINT_EXPECT_VERSION}; no version found"
        exit 1
    fi
fi
HLINT_ACTUAL_VERSION=$(${HLINT} --version | head -n 1 | cut -d' ' -f 2 | sed -E 's/v(.*),/\1/')
if [ ! "${HLINT_ACTUAL_VERSION}" = "${HLINT_EXPECT_VERSION}" ]; then
    echo "Requires hlint ${HLINT_EXPECT_VERSION}; version ${HLINT_ACTUAL_VERSION} found"
    exit 1
fi

# Lint Haskell source with HLint
echo "Lint Haskell files..."
# shellcheck disable=SC2086
git ls-files --exclude-standard --no-deleted --deduplicate '*.hs' | xargs -L50 ${HLINT}

