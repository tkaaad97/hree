#ifndef SKIN_DEFINED
#define SKIN_DEFINED
#ifdef USE_SKINNING

struct JointMatrixArray {
    mat4 items[MAX_JOINT_COUNT];
};

layout(std140) uniform JointMatricesBlock {
    int count;
    JointMatrixArray data;
} jointMatricesBlock;

layout(std140) uniform JointInverseMatricesBlock {
    int count;
    JointMatrixArray data;
} jointInverseMatricesBlock;

#endif
#endif
