precision mediump float;

varying vec2 vTexCoord;
uniform float u_time;

void main() {
    float wave = sin(vTexCoord.x * 10.0 + u_time) * 0.1; 
    vec3 color = vec3(0.5 + wave, 0.5, 1.0); 
    gl_FragColor = vec4(color, 1.0); 
}
