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

struct LightArray {
    Light items[MAX_LIGHT_COUNT];
};

layout(std140) uniform LightBlock {
    int count;
    LightArray light;
} lightBlock;

const int LightTypeDirectional = 0;
const int LightTypePoint = 1;
const int LightTypeSpot = 2;

#endif
