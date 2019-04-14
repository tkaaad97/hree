varying vec3 fragmentNormal;
varying vec2 fragmentUv;
varying vec4 fragmentColor;
uniform sampler2D texture;

void main()
{
    gl_FragColor = fragmentColor;
}
