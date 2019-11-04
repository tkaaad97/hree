in vec3 fragmentPosition;
in vec3 fragmentNormal;
in vec2 fragmentUv;
#ifdef HAS_VERTEX_COLOR
in vec4 fragmentColor;
#endif

out vec4 outColor;

uniform mat4 viewMatrix = mat4(1.0);
uniform vec3 viewPosition = vec3(0.0);
uniform vec4 baseColor = vec4(1.0, 1.0, 1.0, 1.0);
uniform float metalness = 0.5;
uniform float roughness = 0.5;
uniform vec3 emissiveness = vec3(0.0, 0.0, 0.0);
uniform sampler2D texture;
uniform vec3 directionalLight = vec3(0.0, 0.0, 0.0);

const float pi = 3.141592653589793;
const float epsilon = 1.19209e-07;
const vec3 dielectricSpecular = vec3(0.04, 0.04, 0.04);
const vec3 black = vec3(0.0, 0.0, 0.0);

vec3 diffuse(vec3 a)
{
    return a / pi;
}

vec3 calcFresnelSchlick(vec3 base, float metalness, float dotVH)
{
    vec3 f0 = mix(dielectricSpecular, base, metalness);
    vec3 F = f0 + (1.0 - f0) * pow(clamp(1.0 - dotVH, 0.0, 1.0), 5.0);
    return F;
}

float calcVisibilityOcclusion(float alpha2, float dotNL, float dotNV)
{
    float GGXV = dotNL * sqrt(dotNV * dotNV * (1.0 - alpha2) + alpha2);
    float GGXL = dotNV * sqrt(dotNL * dotNL * (1.0 - alpha2) + alpha2);
    float GGX = GGXV + GGXL;
    if (GGX > 0.0) {
        return 0.5 / (GGXV + GGXL);
    }
    return 0.0;
}

float calcMicrofacetDistribution(float alpha2, float dotNH)
{
    float f = dotNH * (dotNH * alpha2 - dotNH) + 1.0;
    float D = alpha2 / max(pi * f * f, epsilon);
    return D;
}

vec3 calcPointShade(vec3 pointToLight, vec3 normal, vec3 view, vec3 color, float metalness, float roughness)
{
    float alpha = roughness * roughness;
    float alpha2 = alpha * alpha;
    vec3 n = normalize(normal);
    vec3 v = normalize(view);
    vec3 l = normalize(pointToLight);
    vec3 h = normalize(l + v);
    float dotNL = clamp(dot(n, l), 0.0, 1.0);
    float dotNV = clamp(dot(n, v), 0.0, 1.0);
    float dotNH = clamp(dot(n, h), 0.0, 1.0);
    float dotVH = clamp(dot(v, h), 0.0, 1.0);

    if (dotNL > 0.0 || dotNV > 0.0) {
        vec3 F = calcFresnelSchlick(color, metalness, dotVH);
        float V = calcVisibilityOcclusion(alpha2, dotNL, dotNV);
        float D = calcMicrofacetDistribution(alpha2, dotNH);

        vec3 diffuseOut = (1.0 - F) * diffuse(color);
        vec3 specularOut = F * V * D;
        return dotNL * (diffuseOut + specularOut);
    }
    return vec3(0.0);
}

vec3 applyDirectionalLight(vec3 lightDir, vec3 normal, vec3 view, vec3 color, float metalness, float roughness)
{
    vec3 pointToLight = -lightDir;
    return calcPointShade(pointToLight, normal, view, color, metalness, roughness);
}

vec3 toneMapping(vec3 color)
{
    const float invGamma = 1.0 / 2.2;
    return pow(color, vec3(invGamma));
}

void main()
{
    float metalnessClamped = clamp(metalness, 0.0, 1.0);
    float roughnessClamped = clamp(roughness, 0.0, 1.0);
    vec3 view = -(viewMatrix * vec4(fragmentPosition, 1.0)).xyz;
    vec3 normal = (viewMatrix * vec4(fragmentNormal, 0.0)).xyz;
    vec3 light = (viewMatrix * vec4(directionalLight, 0.0)).xyz;
    vec4 color = texture2D(texture, fragmentUv) * baseColor;
#ifdef HAS_VERTEX_COLOR
    color = color * fragmentColor;
#endif
    vec3 diffuseColor = mix(a.rgb * (1.0 - dielectricSpecular.r), black, metalnessClamped);

    outColor = vec3(0.0, 0.0, 0.0);
    outColor += applyDirectionalLight(light, normal, view, diffuseColor, metalnessClamped, roughnessClamped);
    outColor = vec4(toneMapping(outColor), 1.0);
}
