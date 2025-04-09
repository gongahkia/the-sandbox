attribute vec3 aPosition;
varying vec3 myColor;

uniform vec3 u_color;

void main() {
    myColor = u_color; 
    gl_Position = vec4(aPosition, 1.0); 
}
