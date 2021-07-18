in vec2 fragmentUv;
in float opacityFactor;

out vec4 outColor;

uniform sampler2D baseColorMapping;

void main()
{
    outColor = texture2D(baseColorMapping, fragmentUv);
    outColor.w *= opacityFactor;
}
