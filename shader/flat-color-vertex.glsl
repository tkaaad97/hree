in vec3 position;
out vec4 fragmentColor;
#ifdef HAS_JOINT_INDICES
in ivec4 jointIndices;
#endif
#ifdef HAS_JOINT_WEIGHTS
in vec4 jointWeights;
#endif

#include <camerablock.glsl>
#include <skin.glsl>

uniform mat4 modelMatrix = mat4(1.0);

layout(std140) uniform MaterialBlock {
    vec4 color;
};

void main()
{
#if defined(USE_VERTEX_SKINNING) && defined(HAS_JOINT_INDICES) && defined(HAS_JOINT_WEIGHTS)
    mat4 transformMatrix =
        jointWeights.x * jointMatricesBlock.items[jointIndices.x] * jointInverseBindMatricesBlock.items[jointIndices.x] +
        jointWeights.y * jointMatricesBlock.items[jointIndices.y] * jointInverseBindMatricesBlock.items[jointIndices.y] +
        jointWeights.z * jointMatricesBlock.items[jointIndices.z] * jointInverseBindMatricesBlock.items[jointIndices.z] +
        jointWeights.w * jointMatricesBlock.items[jointIndices.w] * jointInverseBindMatricesBlock.items[jointIndices.w];
#else
    mat4 transformMatrix = modelMatrix;
#endif
    vec4 modelPosition = transformMatrix * vec4(position, 1.0);
    gl_Position = cameraBlock.projectionMatrix * cameraBlock.viewMatrix * modelPosition;
    fragmentColor = color;
}
