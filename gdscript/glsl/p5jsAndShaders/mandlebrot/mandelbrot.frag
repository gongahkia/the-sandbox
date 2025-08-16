precision mediump float;

varying vec2 vTexCoord;
uniform float u_time;
uniform vec2 u_resolution;

void main() {
    vec2 c = (vTexCoord - 0.5) * 3.0; 
    vec2 z = vec2(0.0);
    int iterations = 0;
    const int maxIterations = 100;

    for (int i = 0; i < maxIterations; i++) {
        if (length(z) > 2.0) break; 
        z = vec2(z.x * z.x - z.y * z.y, 2.0 * z.x * z.y) + c; 
        iterations++;
    }

    float colorValue = float(iterations) / float(maxIterations); 
    gl_FragColor = vec4(vec3(colorValue), 1.0); 
}
