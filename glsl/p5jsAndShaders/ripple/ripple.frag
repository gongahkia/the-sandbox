precision mediump float;

varying vec2 vTexCoord;
uniform float u_time;
uniform vec2 u_mouse;

void main() {
    float distance = length(vTexCoord - u_mouse); 
    float ripple = sin(distance * 10.0 - u_time * 5.0) * 0.05; 
    vec3 color = vec3(0.0, 0.5 + ripple, 1.0); 
    gl_FragColor = vec4(color, 1.0);
}
