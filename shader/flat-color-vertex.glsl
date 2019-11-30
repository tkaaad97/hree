in vec3 position;
out vec4 fragmentColor;

#include <camerablock.glsl>

uniform mat4 modelMatrix = mat4(1.0);
uniform vec4 color = vec4(1.0);

void main()
{
    gl_Position = camera.projectionMatrix * camera.viewMatrix * modelMatrix * vec4(position, 1.0);
    fragmentColor = color;
}
