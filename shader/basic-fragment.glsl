in vec3 fragmentNormal;
in vec2 fragmentUv;
in vec4 fragmentColor;

out vec4 outColor;

uniform sampler2D texture;
uniform vec3 directionalLight = vec3(0.0, 0.0, 0.0);

void main()
{
    float diffuse = clamp(dot(fragmentNormal, -normalize(directionalLight)), 0.1, 1.0);
    vec3 color = texture2D(texture, fragmentUv).rgb;
    color = mix(color * fragmentColor.rgb, color, fragmentColor.a);
    outColor = diffuse * vec4(color, 1.0);
}
