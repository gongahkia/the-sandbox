let angle = 0;
let img;

function preload() {
    img = loadImage('https://encrypted-tbn0.gstatic.com/images?q=tbn:ANd9GcTfz66RVsvJulZ9sddfxzQe8PDnoWu8m5Zl36XkMKzTUUOJuTUCUV30KLteCwcGi2HSFKw&usqp=CAU');
}

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
    for (let i = 0; i < 6; i++) {
        let x = 100 * cos(TWO_PI * i / 6);
        let y = 100 * sin(TWO_PI * i / 6);
        vertex(x, y, -100);
    }
    endShape(CLOSE);

    beginShape();
    for (let i = 0; i < 6; i++) {
        let x = 100 * cos(TWO_PI * i / 6);
        let y = 100 * sin(TWO_PI * i / 6);
        vertex(x, y, 100);
    }
    endShape(CLOSE);

    for (let i = 0; i < 6; i++) {
        let x1 = 100 * cos(TWO_PI * i / 6);
        let y1 = 100 * sin(TWO_PI * i / 6);
        let x2 = 100 * cos(TWO_PI * (i + 1) / 6);
        let y2 = 100 * sin(TWO_PI * (i + 1) / 6);

        beginShape();
        texture(img);
        vertex(x1, y1, -100, i / 6, 0);
        vertex(x1, y1, 100, i / 6, 1);
        vertex(x2, y2, 100, (i + 1) / 6, 1);
        vertex(x2, y2, -100, (i + 1) / 6, 0);
        endShape(CLOSE);
    }

    angle += 0.01;
}
