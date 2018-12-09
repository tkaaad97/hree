#version 450

in vec3 fragmentPosition;
in vec3 fragmentNormal;
in vec2 fragmentUv;
in vec4 fragmentColor;
out vec4 outColor;
uniform sampler2D texture;

void main()
{
    outColor = vec4(1.0, 0.0, 0.0, 1.0);
}