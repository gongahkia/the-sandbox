console.log("walao eh");

// creates a canvas object
var canvas = document.getElementById("shitCanvas");
canvas.width = window.innerWidth;
canvas.height = window.innerHeight;
console.log(canvas);

// the variable c refers to the local canvas context, generating a pointer that moves around a given coordinate
var c = canvas.getContext('2d');

// creating a mouse object for the event listener to use

var mouse = {
    x: undefined,
    y: undefined
}

// compartmentalizing the various circle attributes
var maxCircleRadius = 20;
var minCircleRadius = 5;

// an array of random colors that the circles will flash in and out of
var colorArray = [
    'green',
    'red',
    'black',
    'blue',
    'yellow',
    'pink',
    'purple',
    'cyan',
    'orange',
]

// -- Used to arrive at a random integer that determines the color of each circle
// console.log(colorArray[Math.floor(Math.random() * colorArray.length)])

// creating an event listener similar to those in Java, the event tracked being the mouse moving
window.addEventListener('mousemove', 
    function() {
    // console.log(event);
    mouse.x = event.x;
    mouse.y = event.y;
    console.log(mouse);
})

// a javascript 'object' is called with the `function` name call
function Circle(x,y,dx,dy,circleRadius) {

    // Circle object attributes
    this.x = x;
    this.y = y;
    this.dx = dx;
    this.dy = dy;
    this.circleRadius = circleRadius;
    this.color = colorArray[Math.floor(Math.random() * colorArray.length)];

    // Circle object methods
    this.draw = function() {
        console.log("circle has been instantiated and drawn");
        c.beginPath();
        c.arc(this.x,this.y,this.circleRadius,0,Math.PI * 2, false);
        c.strokeStyle = this.color;
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

        // this conditional check enlarges the circles within the given radius of 50 pixels within a mouse
        if (mouse.x - this.x < 50 && mouse.x - this.x > -50 && mouse.y - this.y < 50 && mouse.y - this.y > -50) {
            if (this.circleRadius < maxCircleRadius) {
                this.circleRadius += 1;
            }
        } else if (this.circleRadius > minCircleRadius) {
            this.circleRadius -= 1;
        }

        // final draw method called on the Circle object
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
