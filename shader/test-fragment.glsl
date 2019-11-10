varying vec3 fragmentNormal;
varying vec2 fragmentUv;
varying vec4 fragmentColor;
uniform sampler2D baseColorTexture;

void main()
{
    gl_FragColor = fragmentColor;
}
