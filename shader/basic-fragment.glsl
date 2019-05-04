#version 450

in vec3 fragmentNormal;
in vec2 fragmentUv;
in vec4 fragmentColor = vec4(0.0, 0.0, 0.0, 1.0);

out vec4 outColor;

uniform sampler2D texture = sampler2D(0);

void main()
{
    textureColor = texture(texture, fragmentUv);
    outColor = vec4(mix(fragmentColor.xyz, textureColor.xyz, textureColor.w), fragment.w)
}
