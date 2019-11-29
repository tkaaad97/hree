in vec3 position;
in vec3 normal;
in vec2 uv;
in vec4 color;

out vec3 fragmentNormal;
out vec2 fragmentUv;
#ifdef HAS_VERTEX_COLOR
out vec4 fragmentColor;
#endif

layout(std140) uniform CameraBlock {
    mat4 projectionMatrix;
    mat4 viewMatrix;
    vec3 viewPosition;
} camera;

uniform mat4 modelMatrix = mat4(1.0);

void main()
{
    gl_Position = camera.projectionMatrix * camera.viewMatrix * modelMatrix * vec4(position, 1.0);
    fragmentNormal = normalize((modelMatrix * vec4(normal, 0.0)).xyz);
    fragmentUv = uv;
#ifdef HAS_VERTEX_COLOR
    fragmentColor = color;
#endif
}
