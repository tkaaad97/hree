#!/bin/bash

set -ue -o pipefail

if [ "$#" -ne 2 ]; then
    echo "Usage: $0 GHC_VERSION CABAL_VERSION" >&2
    exit 1
fi

GHC_VERSION="$1"
TARGET_GHC="ghc-${GHC_VERSION}"
export GHC_VERSION

CABAL_VERSION="$2"
TARGET_CABAL="cabal-install-${CABAL_VERSION}"
export CABAL_VERSION

GHC_PACKAGES="${TARGET_GHC} ${TARGET_GHC}-prof ${TARGET_GHC}-dyn ${TARGET_GHC}-htmldocs ${TARGET_CABAL}"
export GHC_PACKAGES

BASE_VERSION=$(find docker/versions -mindepth 1 -maxdepth 1 -type d -print0 | xargs -0 -n1 basename | awk -v "version=$GHC_VERSION" '{ pattern="^"$1 ; if (version ~ pattern) print }' | sort -V | tail -n1)
if [ "$BASE_VERSION" = "" ]; then
    printf "env not found" >&2
    exit 1;
fi

export DOLLAR='$'
# shellcheck source=/dev/null
set -a && source "docker/versions/$BASE_VERSION/env" && set +a && envsubst < docker/Dockerfile.template > "docker/Dockerfile.${TARGET_GHC}"
