in vec3 position;
in vec3 normal;
in vec2 uv;
in vec4 color;

out vec3 fragmentPosition;
out vec3 fragmentNormal;
out vec2 fragmentUv;
#ifdef HAS_VERTEX_COLOR
out vec4 fragmentColor;
#endif

uniform mat4 projectionMatrix = mat4(1.0);
uniform mat4 viewMatrix = mat4(1.0);
uniform mat4 modelMatrix = mat4(1.0);

void main()
{
    vec4 modelPosition = modelMatrix * vec4(position, 1.0);
    gl_Position = projectionMatrix * viewMatrix * modelPosition;
    fragmentPosition = modelPosition.xyz;
    fragmentNormal = normalize((modelMatrix * vec4(normal, 0.0)).xyz);
    fragmentUv = uv;
#ifdef HAS_VERTEX_COLOR
    fragmentColor = color;
#endif
}
