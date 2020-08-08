#!/bin/bash

set -eux

draco_dir="$1"
build_dir="${draco_dir}/build"

mkdir -p "${build_dir}"
stack exec sh -- -c "cd ${build_dir} && cmake -DCMAKE_C_COMPILER=\$(which gcc) -DCMAKE_CXX_COMPILER=\$(which g++) -DCMAKE_CXX_FLAGS=-fPIC -DCMAKE_MAKE_PROGRAM=\$(which make) -DCMAKE_BUILD_TYPE=release .. && make"
