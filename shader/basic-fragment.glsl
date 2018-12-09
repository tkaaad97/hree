#version 450

in vec3 fragmentNormal;
in vec2 fragmentUv;
in vec4 fragmentColor;
out vec4 outColor;
uniform sampler2D texture;

void main()
{
    float f = dot(fragmentNormal, vec3(0.00001)) + dot(fragmentUv, vec2(0.00001)) + dot(fragmentColor, vec4(0.00001));
    outColor = vec4(1.0, 0.0, 0.0, 1.0 + f);
}