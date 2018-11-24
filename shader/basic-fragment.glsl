#version 450

in vec3 fragmentPosition;
in vec3 fragmentNormal;
in vec2 fragmentUv;
in vec2 fragmentColor;
out vec4 outColor;
uniform sampler2D texture;

void main()
{
    outColor = texture(texture, fragmentColor);
}