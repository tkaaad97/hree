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

#include <camerablock.glsl>

uniform mat4 modelMatrix = mat4(1.0);

struct Tile {
    bool flippedHorizontally;
    bool flippedVertically;
    vec2 uv;
    vec2 uvSize;
};

struct TileArray {
    int count;
    Tile items[MAX_SPRITE_TILE_COUNT];
};

layout(std140) uniform MaterialBlock {
    vec3 rotateAxis;
    vec2 uvOffset;
    bool uvFlippedHorizontally;
    bool uvFlippedVertically;
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
    vec3 offset = position + center + rotateMatrix(materialBlock.rotateAxis, angle) * vec3(size.x * positionOffset.x - center.x, size.y * positionOffset.y - center.y, size.z * positionOffset.z - center.z);
    gl_Position = cameraBlock.projectionMatrix * cameraBlock.viewMatrix * modelMatrix * vec4(offset, 1.0);

    bool flipH = (useTile > 0) ? materialBlock.spriteTileArray.items[tileIndex].flippedHorizontally : materialBlock.uvFlippedHorizontally;
    bool flipV = (useTile > 0) ? materialBlock.spriteTileArray.items[tileIndex].flippedVertically : materialBlock.uvFlippedVertically;
    vec2 uv1 = (useTile > 0) ? materialBlock.spriteTileArray.items[tileIndex].uv : uv;
    vec2 uvSize1 = (useTile > 0) ? materialBlock.spriteTileArray.items[tileIndex].uvSize : uvSize;
    float uvoffx = flipH ? (1.0 - uvOffset.x) : uvOffset.x;
    float uvoffy = flipV ? (1.0 - uvOffset.y) : uvOffset.y;
    float uvx = uv1.x + uvoffx * uvSize1.x;
    float uvy = uv1.y + uvoffy * uvSize1.y;
    fragmentUv = materialBlock.uvOffset + vec2(uvx, uvy);
}
