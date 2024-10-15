#!/bin/sh

# Check for cabal
CABAL_EXPECT_VERSION=$(awk -F'=' '/^cabal=/{print$2}' ./scripts/dev-dependencies.txt)
if [ "${CABAL}" = "" ]; then
    CABAL=$(which "cabal")
    if [ "${CABAL}" = "" ]; then
        echo "Requires cabal ${CABAL_EXPECT_VERSION}; no version found"
        exit 1
    fi
fi
CABAL_ACTUAL_VERSION=$(${CABAL} --numeric-version)
if [ ! "${CABAL_ACTUAL_VERSION}" = "${CABAL_EXPECT_VERSION}" ]; then
    echo "Requires cabal ${CABAL_EXPECT_VERSION}; version ${CABAL_ACTUAL_VERSION} found"
    exit 1
fi

# POSIX compliant method for 'pipefail':
FAIL=$(mktemp)

# Check Cabal files with cabal
echo "Lint Cabal files..."
echo
# shellcheck disable=SC2046
set -- $(git ls-files --exclude-standard --no-deleted --deduplicate '*.cabal')
CWD=$(pwd)
for PACKAGE_CABAL_FILE; do
    PACKAGE_DIR=$(dirname "${PACKAGE_CABAL_FILE}")
    echo "Lint ${PACKAGE_DIR}"
    cd "${PACKAGE_DIR}" && ${CABAL} check -ilicense-none || echo > "${FAIL}"
    cd "${CWD}" || exit 2
    echo
done

# Check whether or not any subcommand failed:
if [ -s "${FAIL}" ]; then
    rm "${FAIL}"
    exit 1
else
    rm "${FAIL}"
    exit 0
fi
