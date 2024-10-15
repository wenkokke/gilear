#!/bin/sh

# Get the reference to which to compare the current state.
# If no argument is given, the staged changes are compared to the local HEAD.
# If an argument is given, the local HEAD is compared to the reference.
GITREF="${1}"
if [ "${GITREF}" = "" ]; then
GITREF="--staged"
fi

# POSIX compliant method for 'pipefail':
FAIL=$(mktemp)

# Iterate over all directories with a CHANGELOG.md file
# shellcheck disable=SC2046
set -- $(git ls-files --exclude-standard --no-deleted --deduplicate '**/CHANGELOG.md')
for CHANGELOG; do
    PACKAGE=$(dirname "${CHANGELOG}")
    echo "Lint ${PACKAGE}"
    if git diff --exit-code --quiet "${GITREF}" -- "${CHANGELOG}"; then
        if ! git diff --exit-code --quiet "${GITREF}" -- "${PACKAGE}"; then
            echo "WARNING: ${PACKAGE} has changes, but its CHANGELOG.md was not modified"
            echo > "${FAIL}"
        fi
    fi
done

# Check whether or not any subcommand failed:
if [ -s "${FAIL}" ]; then
    rm "${FAIL}"
    exit 1
else
    rm "${FAIL}"
    exit 0
fi
