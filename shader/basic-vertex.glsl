#version 450
#define MAX_BONES 256

in vec3 position;
in vec3 normal;
in vec2 uv;
in vec4 color;
in vec4 boneIndex;
in vec4 boneWeight;

out vec3 fragmentNormal;
out vec2 fragmentUv;
out vec4 fragmentColor;

uniform mat4 projectionViewMatrix = mat4(1.0);
uniform mat4 modelMatrix = mat4(1.0);
uniform mat4 boneMatrices[ MAX_BONES ];

void main()
{
    mat4 boneMatrix =
        boneWeight.x * boneMatrices[int(jointIndex.x)] +
        boneWeight.y * boneMatrices[int(jointIndex.y)] +
        boneWeight.z * boneMatrices[int(jointIndex.z)] +
        boneWeight.w * boneMatrices[int(jointIndex.w)] +
        clamp(1.0 - boneWeight.x - boneWeight.y - boneWeight.z - boneWeight.z, 0.0, 1.0) * mat4(1.0);
    gl_Position = projectionViewMatrix * modelMatrix * boneMatrix * vec4(position, 1.0);
    fragmentNormal = normal;
    fragmentUv = uv;
    fragmentColor = color;
}
