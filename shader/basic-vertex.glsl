#version 450

in vec3 position;
in vec3 normal;
in vec2 uv;
in vec4 color;

out vec3 fragmentPosition;
out vec3 fragmentNormal;
out vec2 fragmentUv;
out vec4 fragmentColor;

uniform mat4 projectionViewMatrix;
uniform mat4 modelMatrix;

void main()
{
    fragmentPosition = (projectionViewMatrix * modelMatrix * vec4(position, 1.0)).xyz;
    fragmentNormal = normal;
    fragmentUv = uv;
    fragmentColor = color;
}