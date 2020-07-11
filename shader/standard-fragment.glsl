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

#include <camerablock.glsl>
#include <lightblock.glsl>

struct AngularInfo {
    float dotNL;
    float dotNV;
    float dotNH;
    float dotVH;
};

struct MaterialInfo {
    vec3 color;
    vec3 albedoColor;
    float metallic;
    float roughness;
    float alpha;
    float alpha2;
};

layout(std140) uniform MaterialBlock {
    vec4 baseColorFactor;
    vec3 emissiveFactor;
    float metallicFactor;
    float roughnessFactor;
    float normalScale;
    float occlusionStrength;
} materialBlock;

uniform sampler2D baseColorTexture;
uniform sampler2D normalTexture;
uniform sampler2D metallicRoughnessTexture;
uniform sampler2D emissiveTexture;
uniform sampler2D occlusionTexture;

const float pi = 3.141592653589793;
const float epsilon = 1.19209e-07;
const vec3 dielectricSpecular = vec3(0.04, 0.04, 0.04);
const vec3 black = vec3(0.0, 0.0, 0.0);

vec3 getNormal() {
    vec2 uv = fragmentUv;

#ifndef HAS_VERTEX_TANGENT
    vec3 pdx = dFdx(fragmentPosition);
    vec3 pdy = dFdy(fragmentPosition);
    vec3 tdx = dFdx(vec3(uv, 0.0));
    vec3 tdy = dFdy(vec3(uv, 0.0));

#ifdef HAS_VERTEX_NORMAL
    vec3 ng = normalize(fragmentNormal);
#else
    vec3 ng = normalize(cross(pdx, pdy));
#endif

    vec3 t = normalize((tdy.t * pdx - tdx.t * pdy) / (tdx.s * tdy.t - tdy.s * tdx.t));
    t = normalize(t - ng * dot(ng, t));
    vec3 b = normalize(cross(ng, t));
    mat3 tbn = mat3(t, b, ng);
#else
    mat3 tbn = mat3(normalize(fragmentTBN[0]), normalize(fragmentTBN[1]), normalize(fragmentTBN[2]));
#endif

#ifdef HAS_NORMAL_MAP
    vec3 n = texture2D(normalTexture, uv).rgb;
    n = normalize(tbn * ((2.0 * n - 1.0) * vec3(materialBlock.normalScale, materialBlock.normalScale, 1.0)));
#else
    vec3 n = normalize(tbn[2].xyz);
#endif
    return n;
}

vec3 diffuse(vec3 a) {
    return a / pi;
}

vec3 calcFresnelSchlick(vec3 base, float metallicFactor, float dotVH) {
    vec3 f0 = mix(dielectricSpecular, base, metallicFactor);
    vec3 F = f0 + (1.0 - f0) * pow(clamp(1.0 - dotVH, 0.0, 1.0), 5.0);
    return F;
}

float calcVisibilityOcclusion(float alpha2, float dotNL, float dotNV) {
    float GGXV = dotNL * sqrt(dotNV * dotNV * (1.0 - alpha2) + alpha2);
    float GGXL = dotNV * sqrt(dotNL * dotNL * (1.0 - alpha2) + alpha2);
    float GGX = GGXV + GGXL;
    if (GGX > 0.0) {
        return 0.5 / GGX;
    }
    return 0.0;
}

float calcMicrofacetDistribution(float alpha2, float dotNH) {
    float f = dotNH * dotNH * (alpha2 - 1.0) + 1.0;
    float D = alpha2 / max(pi * f * f, epsilon);
    return D;
}

vec3 calcPointShade(AngularInfo angular, MaterialInfo material) {
    if (angular.dotNL > 0.0 || angular.dotNV > 0.0) {
        vec3 F = calcFresnelSchlick(material.color, material.metallic, angular.dotVH);
        float V = calcVisibilityOcclusion(material.alpha2, angular.dotNL, angular.dotNV);
        float D = calcMicrofacetDistribution(material.alpha2, angular.dotNH);

        vec3 diffuseOut = (1.0 - F) * diffuse(material.albedoColor);
        vec3 specularOut = F * V * D;
        return angular.dotNL * (diffuseOut + specularOut);
    }
    return vec3(0.0);
}

vec3 applyDirectionalLight(Light light, AngularInfo angular, MaterialInfo material) {
    return light.intensity * light.color * calcPointShade(angular, material);
}

vec3 applyLights(vec3 normal, vec3 view, vec3 color, float metallic, float roughness) {
    vec3 acc = vec3(0.0);
    float alpha = roughness * roughness;
    float alpha2 = alpha * alpha;
    vec3 n = normalize(normal);
    vec3 v = normalize(view);
    float dotNV = clamp(dot(n, v), 0.0, 1.0);
    vec3 albedoColor = mix(color * (1.0 - dielectricSpecular.r), black, metallic);
    MaterialInfo material = MaterialInfo(color, albedoColor, metallic, roughness, alpha, alpha2);

    for (int i = 0; i < MAX_LIGHT_COUNT; i++) {
        if (i < lightBlock.count) {
            Light light = lightBlock.light.items[i];
            if (light.type == LightTypeDirectional) {
                vec3 l = normalize(- light.direction);
                vec3 h = normalize(l + v);
                float dotNL = clamp(dot(n, l), 0.0, 1.0);
                float dotNH = clamp(dot(n, h), 0.0, 1.0);
                float dotVH = clamp(dot(v, h), 0.0, 1.0);
                AngularInfo angular = AngularInfo(dotNL, dotNV, dotNH, dotVH);
                acc += applyDirectionalLight(light, angular, material);
            }
        }
    }
    return acc;
}

vec4 sRGBToLinear(vec4 color) {
    const float gamma = 2.2;
    return vec4(pow(color.rgb, vec3(gamma)), color.a);
}

vec3 toneMapping(vec3 color) {
    const float invGamma = 1.0 / 2.2;
    return pow(color, vec3(invGamma));
}

void main() {
    float metallic = clamp(materialBlock.metallicFactor, 0.0, 1.0);
    float roughness = clamp(materialBlock.roughnessFactor, 0.0, 1.0);
    vec3 view = cameraBlock.viewPosition - fragmentPosition;
    vec3 normal = getNormal();
    vec4 color = sRGBToLinear(texture2D(baseColorTexture, fragmentUv)) * materialBlock.baseColorFactor;

#ifdef HAS_VERTEX_COLOR
    color *= fragmentColor;
#endif

#ifdef HAS_METALLIC_ROUGHNESS_MAP
    vec4 mrSample = texture2D(metallicRoughnessTexture, fragmentUv);
    metallic = mrSample.b * materialBlock.metallicFactor;
    roughness = mrSample.g * materialBlock.roughnessFactor;
#endif

    vec3 acc = vec3(0.0, 0.0, 0.0);
    acc += applyLights(normal, view, color.rgb, metallic, roughness);

#ifdef HAS_EMISSIVE_MAP
    vec3 emissive = sRGBToLinear(texture(emissiveTexture, fragmentUv)).rgb * materialBlock.emissiveFactor;
    acc += emissive;
#endif

#ifdef HAS_OCCLUSION_MAP
    float ao = texture(occlusionTexture, fragmentUv).r;
    acc = mix(acc, acc * ao, materialBlock.occlusionStrength);
#endif

    outColor = vec4(toneMapping(acc), 1.0);
}
