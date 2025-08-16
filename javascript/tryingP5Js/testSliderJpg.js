let img;
let imgLoaded = false;

function preload() {
  img = loadImage('https://encrypted-tbn0.gstatic.com/images?q=tbn:ANd9GcTfz66RVsvJulZ9sddfxzQe8PDnoWu8m5Zl36XkMKzTUUOJuTUCUV30KLteCwcGi2HSFKw&usqp=CAU', () => {
    imgLoaded = true;
  });
}

function setup() {
  createCanvas(600, 600);
}

function draw() {
  background(220);
  if (!imgLoaded) {
    textSize(32);
    fill(0);
    textAlign(CENTER, CENTER);
    text("Loading Image...", width / 2, height / 2);
    return;
  }
  image(img, 0, 0, width, height); 
}
