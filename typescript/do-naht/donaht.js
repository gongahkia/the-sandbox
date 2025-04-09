function donut() {
    var A_spacing = 0.04;
    var B_spacing = 0.02;
    var A = 0;
    var B = 0;
    var width = 80;
    var height = 22;
    var zBuffer = new Array(width * height);
    var outputBuffer = new Array(width * height);
    var renderDonut = function () {
        console.clear();
        for (var k = 0; k < width * height; k++) {
            zBuffer[k] = 0;
            outputBuffer[k] = " ";
        }
        for (var j = 0; j < 2 * Math.PI; j += 0.07) {
            for (var i = 0; i < 2 * Math.PI; i += 0.02) {
                var sinA = Math.sin(A);
                var cosA = Math.cos(A);
                var sinB = Math.sin(B);
                var cosB = Math.cos(B);
                var sini = Math.sin(i);
                var cosi = Math.cos(i);
                var sinj = Math.sin(j);
                var cosj = Math.cos(j);
                var cosj2 = cosj + 2;
                var mess = 1 / (sini * cosj2 * sinA + sinj * cosA + 5);
                var t = sini * cosj2 * cosA - sinj * sinA;
                var x = Math.floor(width / 2 + width * 0.4 * mess * (cosi * cosj2 * cosB - t * sinB));
                var y = Math.floor(height / 2 + height * 0.3 * mess * (cosi * cosj2 * sinB + t * cosB));
                var o = x + width * y;
                var N = Math.floor(8 * ((sinj * sinA - sini * cosj * cosA) * cosB - sini * cosj * sinA - sinj * cosA - cosi * cosj * sinB));
                if (height > y && y > 0 && x > 0 && width > x && mess > zBuffer[o]) {
                    zBuffer[o] = mess;
                    outputBuffer[o] = ".,-~:;=!*#$@"[Math.max(0, N)];
                }
            }
        }
        process.stdout.write("\x1b[H");
        for (var k = 0; k < outputBuffer.length; k++) {
            process.stdout.write(k % width ? outputBuffer[k] : "\n");
        }
    };
    setInterval(function () {
        renderDonut();
        A += A_spacing;
        B += B_spacing;
    }, 50);
}
donut();
