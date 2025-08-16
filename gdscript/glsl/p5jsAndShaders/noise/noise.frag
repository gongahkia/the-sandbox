precision mediump float;

varying vec2 vTexCoord;
uniform float u_time;

float noise(vec2 p) {
    return sin(p.x * 10.0 + u_time) * cos(p.y * 10.0 + u_time);
}

void main() {
    float n = noise(vTexCoord * 5.0);
    gl_FragColor = vec4(vec3(n), 1.0); 
}
