attribute vec3 position;
attribute vec3 normal;
attribute vec2 uv;
attribute vec4 color;

varying vec3 fragmentNormal;
varying vec2 fragmentUv;
varying vec4 fragmentColor;

uniform mat4 projectionViewMatrix;
uniform mat4 modelMatrix;

void main()
{
    gl_Position = projectionViewMatrix * modelMatrix * vec4(position, 1.0);
    fragmentColor = color;
    fragmentUv = uv;
    fragmentNormal = normal;
}
