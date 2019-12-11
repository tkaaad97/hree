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
    Light items[MAX_LIGHT_COUNT];
    int count;
} lightBlock;

const int LightTypeDirectional = 0;
const int LightTypePoint = 1;
const int LightTypeSpot = 2;

#endif
