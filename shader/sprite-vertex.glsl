#version 450

in vec3 positionOffset;
in vec2 uvOffset;
in vec3 position;
in vec3 size;
in float angle;
in vec2 uv;
in vec2 uvSize;

out vec2 fragmentUv;

uniform mat4 projectionViewMatrix = mat4(1.0);
uniform mat4 modelMatrix = mat4(1.0);
uniform vec3 rotateAxis = vec3(0.0, 0.0, 1.0);

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
    vec3 offset = position + rotateMatrix(rotateAxis, angle) * vec3(size.x * positionOffset.x, size.y * positionOffset.y, size.z * positionOffset.z);
    gl_Position = projectionViewMatrix * modelMatrix * vec4(offset, 1.0);
    fragmentUv = uv + vec2(uvOffset.x * uvSize.x, uvOffset.y * uvSize.y);
}
