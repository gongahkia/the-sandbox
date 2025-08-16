attribute vec3 aPosition;
varying vec2 vTexCoord;

void main() {
    vTexCoord = (aPosition.xy * 0.5) + 0.5; 
    gl_Position = vec4(aPosition, 1.0); 
}