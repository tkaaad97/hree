#!/bin/bash

set -eu

draco_dir="$1"
build_dir="${draco_dir}/build"

mkdir "$build_dir"
cd "$build_dir"
stack exec sh -- -c "cmake -DCMAKE_CXX_COMPILER=\$(which g++) -DCMAKE_CXX_FLAGS=-fPIC -DCMAKE_MAKE_PROGRAM=\$(which make) -DCMAKE_BUILD_TYPE=release .."
stack exec make
