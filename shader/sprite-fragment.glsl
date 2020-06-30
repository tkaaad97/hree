in vec2 fragmentUv;
in float opacityFactor;

out vec4 outColor;

uniform sampler2D baseColorTexture;

void main()
{
    outColor = texture2D(baseColorTexture, fragmentUv);
    outColor.w *= opacityFactor;
}
