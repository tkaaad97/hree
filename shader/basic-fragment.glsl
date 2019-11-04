in vec3 fragmentNormal;
in vec2 fragmentUv;
#ifdef HAS_VERTEX_COLOR
in vec4 fragmentColor;
#endif

out vec4 outColor;

uniform sampler2D texture;
uniform vec3 directionalLight = vec3(0.0, 0.0, 0.0);

void main()
{
    float diffuse = clamp(dot(fragmentNormal, -normalize(directionalLight)), 0.1, 1.0);
    vec4 color = texture2D(texture, fragmentUv);
#ifdef HAS_VERTEX_COLOR
    color = color * fragmentColor;
#endif
    outColor = diffuse * color;
}
