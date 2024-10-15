#!/bin/sh

# Check for ShellCheck
SHELLCHECK_EXPECT_VERSION=$(awk -F'=' '/^shellcheck=/{print$2}' ./scripts/dev-dependencies.txt)
if [ "${SHELLCHECK}" = "" ]; then
    SHELLCHECK=$(which "shellcheck")
    if [ "${SHELLCHECK}" = "" ]; then
        echo "Requires ShellCheck ${SHELLCHECK_EXPECT_VERSION}; no version found"
        exit 1
    fi
fi
SHELLCHECK_ACTUAL_VERSION=$(${SHELLCHECK} --version | head -n 2 | tail -n 1 | cut -d' ' -f2)
if [ ! "${SHELLCHECK_ACTUAL_VERSION}" = "${SHELLCHECK_EXPECT_VERSION}" ]; then
    echo "Requires ShellCheck ${SHELLCHECK_EXPECT_VERSION}; version ${SHELLCHECK_ACTUAL_VERSION} found"
    exit 1
fi

# Check scripts with ShellCheck
echo "Lint Shell scripts..."
# shellcheck disable=SC2086
if ! git ls-files --exclude-standard --no-deleted --deduplicate '*.sh' | xargs -L50 ${SHELLCHECK} -s sh; then
    exit 1
fi
