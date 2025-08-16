console.log("walao eh");

// creates a canvas object
var canvas = document.getElementById("shitCanvas");
canvas.width = window.innerWidth;
canvas.height = window.innerHeight;
console.log(canvas);

// the variable c refers to the local canvas context, generating a pointer that moves around a given coordinate
var c = canvas.getContext('2d');

// creates a flat object in the canvas context
// c.fillRect(0,0,100,100);
// c.fillRect(0,200,100,100);
// c.fillRect(200,0,100,100);
// c.fillRect(100,100,100,100);
// c.fillRect(200,200,100,100);
// c.fillStyle = 'rgba(255, 0, 0, 0.5)'

// creates a line in canvas
/* c.beginPath();
var triangleStartingXCoord = Math.random() * window.innerWidth;
var triangleStartingYCoord = Math.random() * window.innerHeight;
c.moveTo(triangleStartingXCoord, triangleStartingYCoord);
c.lineTo(Math.random() * window.innerWidth, Math.random() * window.innerHeight);
c.lineTo(Math.random() * window.innerWidth, Math.random() * window.innerHeight);
c.lineTo(triangleStartingXCoord, triangleStartingYCoord);
c.strokeStyle = "blue";
c.stroke(); */

// drawing a circle
// c.beginPath(); // the beginPath() method restarts the coordinate pointer for the canvas context object
// c.arc(300,300,30,0,Math.PI * 2, false);
// c.stroke();

// drawing multiple circles using a for loop and the random coordinate determiner
/* for (var i = 0; i < 10; i++) {

    var color_weight = Math.random();
    if (color_weight <= 0.25) {
        color = 'blue';
    } else if (color_weight <= 0.50) {
        color = 'red';
    } else if (color_weight <= 0.75) {
       color = 'yellow';
    } else if (color_weight <= 1.00) {
        color = 'green';
    } else {
        console.log("edge case detected");
    }

    // randomises the value of the circles on the screen
    var x = Math.random() * window.innerWidth;
    var y = Math.random() * window.innerHeight;

    c.beginPath(); 
    c.arc(x,y,30,0,Math.PI * 2, false);
    c.strokeStyle = color;
    c.stroke();

}*/

// 1. FIGURE OUT HOW TO MAKE CIRCLE COLOR CHANGE AND MAINTAIN EVERY SINGLE TIME IT HITS A WALL


// a javascript 'object' is called with the `function` name call
function Circle(x,y,dx,dy,circleRadius) {

    // Circle object attributes
    this.x = x;
    this.y = y;
    this.dx = dx;
    this.dy = dy;
    this.circleRadius = circleRadius;

    // Circle object methods
    this.draw = function() {
        console.log("circle has been instantiated and drawn");
        c.beginPath();
        c.arc(this.x,this.y,this.circleRadius,0,Math.PI * 2, false);
        c.strokeStyle = 'blue';
        c.stroke();
    }

    // Another Circle object method
    this.update = function(){
        if (this.x + this.circleRadius > innerWidth || this.x - this.circleRadius < 0) {
            this.dx = -this.dx;
        }
        
        if (this.y + this.circleRadius > innerHeight || this.y - this.circleRadius < 0) { 
            this.dy = -this.dy;
        }
        this.x += this.dx; // currently, X velocity is 4 pixel/second
        this.y += this.dy; // currently  y velocity is 4 pixel/second
        this.draw();
    }
}

var circleArray = [];

for (var i = 0; i < 100; i++) {
    var x = Math.random() * (innerWidth - circleRadius * 2) + circleRadius;
    var y = Math.random() * (innerHeight - circleRadius * 2) + circleRadius;
    var dx = Math.random() - 0.5;
    var dy = Math.random() - 0.5;
    var circleRadius = 30; // determined by drawing of arc 
    circleArray.push(new Circle(x,y,dx,dy,circleRadius));
}

// instantiating a new instance of the Circle object
// var circle = new Circle(200,200,5,5,30);

// main animation function loop refreshes the page repeatedly to simulate animation as the coordinates change
// animate() function
function animate() {
    requestAnimationFrame(animate); // recursive function call
    // we clear the screen first, then update the circle data and redraw it
    c.clearRect(0,0, innerWidth, innerHeight);
    for (var i =0; i < circleArray.length; i++) {
        circleArray[i].update();
    }
}

animate();

