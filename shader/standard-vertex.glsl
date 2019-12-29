in vec3 position;
#ifdef HAS_VERTEX_NORMAL
in vec3 normal;
#endif
#ifdef HAS_VERTEX_TANGENT
in vec3 tangent;
#endif
in vec2 uv;
#ifdef HAS_VERTEX_COLOR
in vec4 color;
#endif
#ifdef HAS_JOINT_INDICES
in ivec4 jointIndices;
#endif
#ifdef HAS_JOINT_WEIGHTS
in vec4 jointWeights;
#endif

out vec3 fragmentPosition;
#ifdef HAS_VERTEX_NORMAL
#ifdef HAS_VERTEX_TANGENT
out mat3 fragmentTBN;
#else
out vec3 fragmentNormal;
#endif
#endif
out vec2 fragmentUv;
#ifdef HAS_VERTEX_COLOR
out vec4 fragmentColor;
#endif

#include <camerablock.glsl>
#include <skin.glsl>

uniform mat4 modelMatrix = mat4(1.0);

void main()
{
#if defined(USE_VERTEX_SKINNING) && defined(HAS_JOINT_INDICES) && defined(HAS_JOINT_WEIGHTS)
    mat4 transformMatrix =
        jointWeights.x * jointMatricesBlock.data.items[jointIndices.x] * jointInverseBindMatricesBlock.data.items[jointIndices.x] +
        jointWeights.y * jointMatricesBlock.data.items[jointIndices.y] * jointInverseBindMatricesBlock.data.items[jointIndices.y] +
        jointWeights.z * jointMatricesBlock.data.items[jointIndices.z] * jointInverseBindMatricesBlock.data.items[jointIndices.z] +
        jointWeights.w * jointMatricesBlock.data.items[jointIndices.w] * jointInverseBindMatricesBlock.data.items[jointIndices.w];
#else
    mat4 transformMatrix = modelMatrix;
#endif
    vec4 modelPosition = transformMatrix * vec4(position, 1.0);
    gl_Position = cameraBlock.projectionMatrix * cameraBlock.viewMatrix * modelPosition;
    fragmentPosition = modelPosition.xyz;
    fragmentUv = uv;
#ifdef HAS_VERTEX_NORMAL
#ifdef HAS_VERTEX_TANGENT
    vec3 n = normalize((transformMatrix * vec4(normal, 0.0)).xyz);
    vec3 t = normalize((transformMatrix * vec4(tangent, 0.0)).xyz);
    vec3 b = cross(n, t);
    fragmentTBN = mat3(t, b, n);
#else
    fragmentNormal = normalize((transformMatrix * vec4(normal, 0.0)).xyz);
#endif
#endif

#ifdef HAS_VERTEX_COLOR
    fragmentColor = color;
#endif
}
