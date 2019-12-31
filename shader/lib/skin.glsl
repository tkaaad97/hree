#ifndef SKIN_DEFINED
#define SKIN_DEFINED
#ifdef USE_VERTEX_SKINNING

layout(std140) uniform JointMatricesBlock {
    int count;
    mat4 items[MAX_JOINT_COUNT];
} jointMatricesBlock;

layout(std140) uniform JointInverseBindMatricesBlock {
    int count;
    mat4 items[MAX_JOINT_COUNT];
} jointInverseBindMatricesBlock;

#endif
#endif
