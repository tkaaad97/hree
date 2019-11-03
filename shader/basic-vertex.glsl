in vec3 position;
in vec3 normal;
in vec2 uv;
in vec4 color;

out vec3 fragmentNormal;
out vec2 fragmentUv;
out vec4 fragmentColor;

uniform mat4 projectionMatrix = mat4(1.0);
uniform mat4 viewMatrix = mat4(1.0);
uniform mat4 modelMatrix = mat4(1.0);

void main()
{
    gl_Position = projectionMatrix * viewMatrix * modelMatrix * vec4(position, 1.0);
    fragmentNormal = (modelMatrix * vec4(normal, 0.0)).xyz;
    fragmentUv = uv;
    fragmentColor = color;
}
