#version 450

in vec3 fragmentPosition;
in vec3 fragmentNormal;
in vec2 fragmentUv;
in vec4 fragmentColor;

out vec4 outColor;

uniform mat4 viewMatrix = mat4(1.0);
uniform vec4 baseColor = vec4(1.0, 1.0, 1.0, 1.0);
uniform float metalness = 0.5;
uniform float roughness = 0.5;
uniform vec3 emissiveness = vec3(0.0, 0.0, 0.0);
uniform sampler2D texture;
uniform vec3 directionalLight = vec3(0.0, 0.0, 0.0);

const float pi = 3.141592653589793;
const float epsilon = 1.0E-6;
const vec3 dieletricSpecular = vec3(0.04, 0.04, 0.04);
const vec3 black = vec3(0.0, 0.0, 0.0);

vec3 diffuse(vec3 a)
{
    return a / pi;
}

vec3 calcFresnelSchlick(vec3 base, float metalness, float dotVH)
{
    vec3 f0 = mix(dieletricSpecular, base, metalness);
    vec3 F = f0 + (vec3(1.0) - f0) * pow(1.0 - dotVH, 5.0);
    return F;
}

float calcVisibilityOcclusion(float alpha2, float dotNL, float dotNV)
{
    float GGXV = dotNL * sqrt(dotNV * dotNV * (1.0 - alpha2) + alpha2);
    float GGXL = dotNV * sqrt(dotNL * dotNL * (1.0 - alpha2) + alpha2);
    return 0.5 / (GGXV + GGXL);
}

float calcMicrofacetDistribution(float alpha2, float dotNH)
{
    float D = alpha2 / max((pi * (dotNH * dotNH * (alpha2 - 1.0) + 1.0)), epsilon);
    return D;
}

vec3 calcPointShade(vec3 pointToLight, vec3 normal, vec3 view, vec3 color, float metalness, float roughness)
{
    float alpha = roughness;
    float alpha2 = alpha * alpha;
    vec3 n = normalize(normal);
    vec3 v = normalize(view);
    vec3 l = normalize(pointToLight);
    vec3 h = normalize(l + v);
    float dotNL = clamp(dot(n, l), 0.0, 1.0);
    float dotNV = clamp(dot(n, v), 0.0, 1.0);
    float dotNH = clamp(dot(n, h), 0.0, 1.0);
    float dotVH = clamp(dot(v, h), 0.0, 1.0);

    vec3 F = calcFresnelSchlick(color, metalness, dotVH);
    float V = calcVisibilityOcclusion(alpha2, dotNL, dotNV);
    float D = calcMicrofacetDistribution(alpha2, dotNH);

    vec3 diffuseOut = (vec3(1.0) - F) * diffuse(color);
    vec3 specularOut = F * V * D;
    return dotNL * (diffuseOut + specularOut);
}

vec3 applyDirectionalLight(vec3 lightDir, vec3 normal, vec3 view, vec3 color, float metalness, float roughness)
{
    vec3 pointToLight = -lightDir;
    return calcPointShade(pointToLight, normal, view, color, metalness, roughness);
}

void main()
{
    vec3 view = (viewMatrix * vec4(fragmentPosition, 1.0)).xyz;
    vec4 a = (texture2D(texture, fragmentUv) * baseColor) * fragmentColor;
    vec3 diffuseColor = a.rgb * (1.0 - metalness);

    vec3 color = vec3(0.0, 0.0, 0.0);
    color += applyDirectionalLight(directionalLight, fragmentNormal, view, diffuseColor, metalness, roughness);
    outColor = vec4(color, 1.0);
}
