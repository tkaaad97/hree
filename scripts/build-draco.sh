#!/bin/bash

set -eux

draco_dir="$1"
build_dir="${draco_dir}/build"

mkdir -p "${build_dir}"
ccompiler=$(stack exec sh -- -c 'which gcc')
cxxcompiler=$(stack exec sh -- -c 'which g++')
makeprogram=$(stack exec sh -- -c 'which make')
stack exec sh -- -c "cd ${build_dir} && cmake -DCMAKE_C_COMPILER=${ccompiler} -DCMAKE_CXX_COMPILER=${cxxcompiler} -DCMAKE_CXX_FLAGS=-fPIC -DCMAKE_MAKE_PROGRAM=${makeprogram} -DCMAKE_BUILD_TYPE=release .. && make"
