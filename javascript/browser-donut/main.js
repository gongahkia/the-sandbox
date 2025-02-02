var canvas = document.getElementById('donutCanvas');
var ctx = canvas.getContext('2d');
canvas.width = window.innerWidth;
canvas.height = window.innerHeight;
var radius1 = 80;
var radius2 = 150;
var K1 = 200;
var K2 = 5;
var A_spacing = 0.07;
var B_spacing = 0.03;
var A = 1.0; // Changed to let
var B = 1.0; // Changed to let
var fps = 30;
var interval = 1000 / fps;
var lastTime = 0;
function drawDonut(A, B) {
    ctx.clearRect(0, 0, canvas.width, canvas.height);
    var sinA = Math.sin(A);
    var cosA = Math.cos(A);
    var sinB = Math.sin(B);
    var cosB = Math.cos(B);
    for (var theta = 0; theta < 2 * Math.PI; theta += A_spacing) {
        var sinTheta = Math.sin(theta);
        var cosTheta = Math.cos(theta);
        for (var phi = 0; phi < 2 * Math.PI; phi += B_spacing) {
            var sinPhi = Math.sin(phi);
            var cosPhi = Math.cos(phi);
            var circleX = radius2 + radius1 * cosTheta;
            var circleY = radius1 * sinTheta;
            var x = circleX * (cosB * cosPhi + sinA * sinB * sinPhi) - circleY * cosA * sinB;
            var y = circleX * (sinB * cosPhi - sinA * cosB * sinPhi) + circleY * cosA * cosB;
            var z = K2 + cosA * circleX * sinPhi + circleY * sinA;
            var ooz = 1 / z;
            var xp = (canvas.width / 2) + K1 * ooz * x;
            var yp = (canvas.height / 2) - K1 * ooz * y;
            var luminance = Math.max(0, cosPhi * cosTheta * sinB - cosA * cosTheta * sinPhi - sinA * sinTheta + cosB * (cosA * sinTheta - cosTheta * sinA * sinPhi));
            ctx.fillStyle = "rgba(255, 255, 255, ".concat(luminance, ")");
            ctx.fillRect(xp, yp, 2, 2);
        }
    }
}
function animate(time) {
    var elapsedTime = time - lastTime;
    if (elapsedTime >= interval) {
        drawDonut(A, B);
        A += 0.07; // A and B are now mutable
        B += 0.03; // A and B are now mutable
        lastTime = time;
    }
    requestAnimationFrame(animate);
}
animate(0);
