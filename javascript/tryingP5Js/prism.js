let angle = 0; 

function setup() {
    createCanvas(600, 600, WEBGL); 
}

function draw() {
    background(220);
    directionalLight(255, 255, 255, 0, -1, -1); 
    ambientLight(100); 
    rotateX(angle);
    rotateY(angle);
    beginShape();
    let radius = 100; 
    let height = 200;
    for (let i = 0; i < 6; i++) {
        let x = radius * cos(TWO_PI * i / 6);
        let y = radius * sin(TWO_PI * i / 6);
        vertex(x, y, -height / 2); 
    }
    endShape(CLOSE);
    beginShape();
    for (let i = 0; i < 6; i++) {
        let x = radius * cos(TWO_PI * i / 6);
        let y = radius * sin(TWO_PI * i / 6);
        vertex(x, y, height / 2); 
    }
    endShape(CLOSE);
    for (let i = 0; i < 6; i++) {
        let x1 = radius * cos(TWO_PI * i / 6);
        let y1 = radius * sin(TWO_PI * i / 6);
        let x2 = radius * cos(TWO_PI * (i + 1) / 6);
        let y2 = radius * sin(TWO_PI * (i + 1) / 6);
        beginShape();
        vertex(x1, y1, -height / 2);
        vertex(x1, y1, height / 2);  
        vertex(x2, y2, height / 2);  
        vertex(x2, y2, -height / 2); 
        endShape(CLOSE);
    }
    angle += 0.01;
}
