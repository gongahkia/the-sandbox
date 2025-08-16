precision mediump float;

varying vec2 vTexCoord; 
uniform float u_time;   
uniform vec2 u_resolution;

void main() {
    vec3 color = 0.5 + 0.5 * cos(u_time + vTexCoord.xyx + vec3(0, 2, 4)); 
    gl_FragColor = vec4(color, 1.0); 
}
