layout(std140) uniform CameraBlock {
    mat4 projectionMatrix;
    mat4 viewMatrix;
    vec3 viewPosition;
} camera;
