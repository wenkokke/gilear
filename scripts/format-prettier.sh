#!/bin/sh

# Check for NPM and prettier
PRETTIER_EXPECT_VERSION=$(awk -F'=' '/^prettier=/{print$2}' ./scripts/dev-dependencies.txt)
if [ "${PRETTIER}" = "" ]; then
    NPM="$(which npm)"
    if [ "${NPM}" = "" ]; then
        echo "Requires npm; no version found"
        exit 1
    fi
    PRETTIER="${NPM} exec prettier --"
fi
PRETTIER_ACTUAL_VERSION=$(NODE_NO_WARNINGS=1 ${PRETTIER} --version | head -n 1)
if [ ! "${PRETTIER_ACTUAL_VERSION}" = "${PRETTIER_EXPECT_VERSION}" ]; then
    echo "Requires prettier ${PRETTIER_EXPECT_VERSION}; version ${PRETTIER_ACTUAL_VERSION} found"
    exit 1
fi

# Format JavaScript, TypeScript, JSON, YAML, and Markdown files with Prettier
echo "Format JavaScript, TypeScript, JSON, YAML, and Markdown files..."
echo
NODE_NO_WARNINGS=1 ${PRETTIER} -w --log-level=warn .
