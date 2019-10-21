#version 450

in vec3 position;
out vec4 fragmentColor;

uniform mat4 projectionMatrix = mat4(1.0);
uniform mat4 viewMatrix = mat4(1.0);
uniform mat4 modelMatrix = mat4(1.0);
uniform vec4 color = vec4(1.0);

void main()
{
    gl_Position = projectionMatrix * viewMatrix * modelMatrix * vec4(position, 1.0);
    fragmentColor = color;
}
