#version 450

in vec3 fragmentNormal;
in vec2 fragmentUv;
in vec4 fragmentColor;

out vec4 outColor;

uniform sampler2D texture;

void main()
{
    vec4 textureColor = texture2D(texture, fragmentUv);
    // outColor = vec4(mix(fragmentColor.xyz, textureColor.xyz, textureColor.w), fragmentColor.w);
    outColor = textureColor;
}
