#ifndef CAMERA_BLOCK_DEFINED
#define CAMERA_BLOCK_DEFINED

layout(std140) uniform CameraBlock {
    mat4 projectionMatrix;
    mat4 viewMatrix;
    vec3 viewPosition;
} cameraBlock;

#endif
