in vec3 fragmentPosition;
#ifdef HAS_VERTEX_NORMAL
#ifdef HAS_VERTEX_TANGENT
in mat3 fragmentTBN;
#else
in vec3 fragmentNormal;
#endif
#endif
in vec2 fragmentUv;
#ifdef HAS_VERTEX_COLOR
in vec4 fragmentColor;
#endif

out vec4 outColor;

layout(std140) uniform CameraBlock {
    mat4 projectionMatrix;
    mat4 viewMatrix;
    vec3 viewPosition;
} camera;

uniform vec4 baseColorFactor = vec4(1.0, 1.0, 1.0, 1.0);
uniform float metallicFactor = 1.0;
uniform float roughnessFactor = 1.0;
uniform vec3 emissiveness = vec3(0.0, 0.0, 0.0);
uniform float normalScale = 1.0;
uniform sampler2D baseColorTexture;
uniform sampler2D normalTexture;
uniform sampler2D metallicRoughnessTexture;
uniform vec3 directionalLight = vec3(0.0, 0.0, 0.0);

const float pi = 3.141592653589793;
const float epsilon = 1.19209e-07;
const vec3 dielectricSpecular = vec3(0.04, 0.04, 0.04);
const vec3 black = vec3(0.0, 0.0, 0.0);

vec3 getNormal()
{
    vec2 uv = fragmentUv;

#ifndef HAS_VERTEX_TANGENT
    vec3 pdx = dFdx(fragmentPosition);
    vec3 pdy = dFdy(fragmentPosition);
    vec3 tdx = dFdx(vec3(uv, 0.0));
    vec3 tdy = dFdy(vec3(uv, 0.0));

#ifdef HAS_VERTEX_NORMAL
    vec3 ng = normalize(fragmentNormal);
#else
    vec3 ng = cross(pdx, pdy);
#endif

    vec3 t = (tdy.t * pdx - tdx.t * pdy) / (tdx.s * tdy.t - tdy.s * tdx.t);
    t = normalize(t - ng * dot(ng, t));
    vec3 b = normalize(cross(ng, t));
    mat3 tbn = mat3(t, b, ng);
#else
    mat3 tbn = fragmentTBN;
#endif

#ifdef HAS_NORMAL_MAP
    vec3 n = texture2D(normalTexture, uv).rgb;
    n = normalize(tbn * ((2.0 * n - 1.0) * vec3(normalScale, normalScale, 1.0)));
#else
    vec3 n = normalize(tbn[2].xyz);
#endif
    return n;
}

vec3 diffuse(vec3 a)
{
    return a / pi;
}

vec3 calcFresnelSchlick(vec3 base, float metallicFactor, float dotVH)
{
    vec3 f0 = mix(dielectricSpecular, base, metallicFactor);
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

vec3 calcPointShade(vec3 pointToLight, vec3 normal, vec3 view, vec3 color, float metallic, float roughness)
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
        vec3 F = calcFresnelSchlick(color, metallic, dotVH);
        float V = calcVisibilityOcclusion(alpha2, dotNL, dotNV);
        float D = calcMicrofacetDistribution(alpha2, dotNH);

        vec3 diffuseOut = (1.0 - F) * diffuse(color);
        vec3 specularOut = F * V * D;
        return dotNL * (diffuseOut + specularOut);
    }
    return vec3(0.0);
}

vec3 applyDirectionalLight(vec3 lightDir, vec3 normal, vec3 view, vec3 color, float metallic, float roughness)
{
    vec3 pointToLight = -lightDir;
    return calcPointShade(pointToLight, normal, view, color, metallic, roughness);
}

vec4 sRGBToLinear(vec4 color)
{
    const float gamma = 2.2;
    return vec4(pow(color.rgb, vec3(gamma)), color.a);
}

vec3 toneMapping(vec3 color)
{
    const float invGamma = 1.0 / 2.2;
    return pow(color, vec3(invGamma));
}

void main()
{
    float metallic = clamp(metallicFactor, 0.0, 1.0);
    float roughness = clamp(roughnessFactor, 0.0, 1.0);
    vec3 view = camera.viewPosition - fragmentPosition;
    vec3 normal = getNormal();
    vec3 light = directionalLight;
    vec4 color = sRGBToLinear(texture2D(baseColorTexture, fragmentUv)) * baseColorFactor;

#ifdef HAS_VERTEX_COLOR
    color *= fragmentColor;
#endif

#ifdef HAS_METALLIC_ROUGHNESS_MAP
    vec4 mrSample = texture2D(metallicRoughnessTexture, fragmentUv);
    metallic = mrSample.b * metallicFactor;
    roughness = mrSample.g * roughnessFactor;
#endif

    vec3 diffuseColor = mix(color.rgb * (1.0 - dielectricSpecular.r), black, metallic);

    vec3 acc = vec3(0.0, 0.0, 0.0);
    acc += applyDirectionalLight(light, normal, view, diffuseColor, metallic, roughness);
    outColor = vec4(toneMapping(acc), 1.0);
}
