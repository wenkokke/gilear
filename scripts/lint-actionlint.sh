#!/bin/sh

# Check for actionlint
ACTIONLINT_EXPECT_VERSION=$(awk -F'=' '/^actionlint=/{print$2}' "./scripts/dev-dependencies.txt")
if [ "${ACTIONLINT}" = "" ]; then
    ACTIONLINT=$(which "actionlint")
    if [ "${ACTIONLINT}" = "" ]; then
        echo "Requires actionlint ${ACTIONLINT_EXPECT_VERSION}; no version found"
        exit 1
    fi
fi
ACTIONLINT_ACTUAL_VERSION=$(${ACTIONLINT} --version | head -n 1)
if [ ! "${ACTIONLINT_ACTUAL_VERSION}" = "${ACTIONLINT_EXPECT_VERSION}" ]; then
    echo "Requires actionlint ${ACTIONLINT_EXPECT_VERSION}; version ${ACTIONLINT_ACTUAL_VERSION} found"
    exit 1
fi

# Run actionlint:
echo "Lint GitHub Actions workflows..."
# shellcheck disable=SC2086
git ls-files --exclude-standard --no-deleted --deduplicate '.github/workflows/*.yaml' | xargs -L50 ${ACTIONLINT}
