in vec3 fragmentNormal;
in vec2 fragmentUv;
#ifdef HAS_VERTEX_COLOR
in vec4 fragmentColor;
#endif

out vec4 outColor;

layout(std140) uniform MaterialBlock {
    vec3 directionalLight;
} materialBlock;

uniform sampler2D baseColorTexture;

void main()
{
    float diffuse = clamp(dot(fragmentNormal, -normalize(materialBlock.directionalLight)), 0.1, 1.0);
    vec4 color = texture2D(baseColorTexture, fragmentUv);
#ifdef HAS_VERTEX_COLOR
    color = color * fragmentColor;
#endif
    outColor = vec4(diffuse * color.xyz, 1.0);
}
