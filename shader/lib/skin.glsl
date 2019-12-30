#ifndef SKIN_DEFINED
#define SKIN_DEFINED
#ifdef USE_VERTEX_SKINNING

struct JointMatrixArray {
    mat4 items[MAX_JOINT_COUNT];
};

layout(std140) uniform JointMatricesBlock {
    int count;
    JointMatrixArray data;
} jointMatricesBlock;

layout(std140) uniform JointInverseBindMatricesBlock {
    int count;
    JointMatrixArray data;
} jointInverseBindMatricesBlock;

#endif
#endif
