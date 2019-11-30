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

uniform mat4 modelMatrix = mat4(1.0);

void main()
{
    vec4 modelPosition = modelMatrix * vec4(position, 1.0);
    gl_Position = camera.projectionMatrix * camera.viewMatrix * modelPosition;
    fragmentPosition = modelPosition.xyz;
    fragmentUv = uv;
#ifdef HAS_VERTEX_NORMAL
#ifdef HAS_VERTEX_TANGENT
    vec3 n = normalize((modelMatrix * vec4(normal, 0.0)).xyz);
    vec3 t = normalize((modelMatrix * vec4(tangent, 0.0)).xyz);
    vec3 b = cross(n, t);
    fragmentTBN = mat3(t, b, n);
#else
    fragmentNormal = normalize((modelMatrix * vec4(normal, 0.0)).xyz);
#endif
#endif

#ifdef HAS_VERTEX_COLOR
    fragmentColor = color;
#endif
}
