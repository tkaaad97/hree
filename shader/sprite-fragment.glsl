in vec2 fragmentUv;

out vec4 outColor;

uniform sampler2D texture;

void main()
{
    outColor = texture2D(texture, fragmentUv);
}
