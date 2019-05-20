#version 450

in vec3 position;
in vec3 positionOffset;
in vec2 uv;

out vec2 fragmentUv;

uniform mat4 projectionViewMatrix = mat4(1.0);
uniform mat4 modelMatrix = mat4(1.0);

void main()
{
    gl_Position = projectionViewMatrix * modelMatrix * vec4(position + positionOffset, 1.0);
    fragmentUv = uv;
}
