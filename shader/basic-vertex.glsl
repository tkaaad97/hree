#version 450

in vec3 position;
in vec3 normal;
in vec2 uv;
in vec4 color;

out vec3 fragmentNormal;
out vec2 fragmentUv;
out vec4 fragmentColor;

uniform mat4 projectionViewMatrix = mat4(1.0);
uniform mat4 modelMatrix = mat4(1.0);

void main()
{
    gl_Position = projectionViewMatrix * modelMatrix * vec4(position, 1.0);
    fragmentNormal = normal;
    fragmentUv = uv;
    fragmentColor = color;
}