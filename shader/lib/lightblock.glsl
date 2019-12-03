#ifndef LIGHT_BLOCK_DEFINED
#define LIGHT_BLOCK_DEFINED

struct Light {
    vec3 direction;
    float range;
    vec3 color;
    float intensity;
    vec3 position;
    float innerConeAngle;
    float outerConeAngle;
    int type;
    vec2 padding;
};

layout(std140) uniform LightBlock {
    Light[MAX_LIGHT_COUNT] items;
    int count;
} light;

#endif
