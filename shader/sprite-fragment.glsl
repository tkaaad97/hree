in vec2 fragmentUv;

out vec4 outColor;

uniform sampler2D baseColorTexture;

void main()
{
    outColor = texture2D(baseColorTexture, fragmentUv);
}
