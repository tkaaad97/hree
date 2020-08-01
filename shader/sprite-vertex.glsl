in vec3 positionOffset;
in vec2 uvOffset;
in vec3 position;
in vec3 size;
in vec3 center;
in float angle;
in vec2 uv;
in vec2 uvSize;
in uint useTile;
in uint tileIndex;

out vec2 fragmentUv;
out float opacityFactor;

#include <camerablock.glsl>

uniform mat4 modelMatrix = mat4(1.0);

struct Tile {
    bool uvFlippedHorizontally;
    bool uvFlippedVertically;
    vec2 uv;
    vec2 uvSize;
};

struct TileArray {
    int count;
    Tile items[MAX_SPRITE_TILE_COUNT];
};

layout(std140) uniform MaterialBlock {
    vec3 rotateAxis;
    float opacityFactor;
    vec3 positionOffset;
    bool uvFlippedHorizontally;
    vec3 sizeFactor;
    bool uvFlippedVertically;
    vec2 uvOffset;
    TileArray spriteTileArray;
} materialBlock;

mat3 rotateMatrix(vec3 axis, float angle)
{
    float cosa = cos(angle);
    float sina = sin(angle);
    vec3 u = normalize(axis);
    mat3 m = mat3(
        cosa + u.x * u.x * (1.0 - cosa),
        u.y * u.x * (1.0 - cosa) + u.z * sina,
        u.z * u.x * (1.0 - cosa) - u.y * sina,
        u.x * u.y * (1.0 - cosa) - u.z * sina,
        cosa + u.y * u.y * (1.0 - cosa),
        u.z * u.y * (1.0 - cosa) + u.x * sina,
        u.x * u.z * (1.0 - cosa) + u.y * sina,
        u.y * u.z * (1.0 - cosa) - u.x * sina,
        cosa + u.z * u.z * (1.0 - cosa));
    return m;
}

void main()
{
    vec3 offset = position + materialBlock.positionOffset
        + center
        + rotateMatrix(materialBlock.rotateAxis, angle) * (size * positionOffset * materialBlock.sizeFactor - center);
    gl_Position = cameraBlock.projectionMatrix * cameraBlock.viewMatrix * modelMatrix * vec4(offset, 1.0);

    bool flipH = (useTile > 0) ? materialBlock.spriteTileArray.items[tileIndex].uvFlippedHorizontally : materialBlock.uvFlippedHorizontally;
    bool flipV = (useTile > 0) ? materialBlock.spriteTileArray.items[tileIndex].uvFlippedVertically : materialBlock.uvFlippedVertically;
    vec2 uv1 = (useTile > 0) ? materialBlock.spriteTileArray.items[tileIndex].uv : uv;
    vec2 uvSize1 = (useTile > 0) ? materialBlock.spriteTileArray.items[tileIndex].uvSize : uvSize;
    vec2 uvoff = vec2(
        flipH ? (1.0 - uvOffset.x) : uvOffset.x,
        flipV ? (1.0 - uvOffset.y) : uvOffset.y);
    fragmentUv = materialBlock.uvOffset + uv1 + uvoff * uvSize1;
    opacityFactor = materialBlock.opacityFactor;
}
