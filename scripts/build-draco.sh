#!/bin/bash

set -eu

draco_dir="$1"
build_dir="${draco_dir}/build"

mkdir -p "${build_dir}"
stack exec bash -- -c "cd ${build_dir} && cmake -DCMAKE_C_COMPILER=\$(which gcc) -DCMAKE_CXX_FLAGS=-fPIC -DCMAKE_EXE_LINKER_FLAGS=-fuse-ld=gold -DCMAKE_CXX_COMPILER=\$(which g++) -DCMAKE_MAKE_PROGRAM=\$(which make) -DCMAKE_BUILD_TYPE=release .. && make"
