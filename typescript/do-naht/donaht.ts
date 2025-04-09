function donut() {
    const A_spacing = 0.04;
    const B_spacing = 0.02;

    let A = 0;
    let B = 0;

    const width = 80;
    const height = 22;
    const zBuffer = new Array<number>(width * height);
    const outputBuffer = new Array<string>(width * height);

    const renderDonut = () => {
        console.clear();

        for (let k = 0; k < width * height; k++) {
            zBuffer[k] = 0;
            outputBuffer[k] = " ";
        }

        for (let j = 0; j < 2 * Math.PI; j += 0.07) {
            for (let i = 0; i < 2 * Math.PI; i += 0.02) {
                const sinA = Math.sin(A);
                const cosA = Math.cos(A);
                const sinB = Math.sin(B);
                const cosB = Math.cos(B);

                const sini = Math.sin(i);
                const cosi = Math.cos(i);
                const sinj = Math.sin(j);
                const cosj = Math.cos(j);

                const cosj2 = cosj + 2;
                const mess = 1 / (sini * cosj2 * sinA + sinj * cosA + 5);
                const t = sini * cosj2 * cosA - sinj * sinA;

                const x = Math.floor(width / 2 + width * 0.4 * mess * (cosi * cosj2 * cosB - t * sinB));
                const y = Math.floor(height / 2 + height * 0.3 * mess * (cosi * cosj2 * sinB + t * cosB));
                const o = x + width * y;
                const N = Math.floor(8 * ((sinj * sinA - sini * cosj * cosA) * cosB - sini * cosj * sinA - sinj * cosA - cosi * cosj * sinB));

                if (height > y && y > 0 && x > 0 && width > x && mess > zBuffer[o]) {
                    zBuffer[o] = mess;
                    outputBuffer[o] = ".,-~:;=!*#$@"[Math.max(0, N)];
                }
            }
        }

        process.stdout.write("\x1b[H");
        for (let k = 0; k < outputBuffer.length; k++) {
            process.stdout.write(k % width ? outputBuffer[k] : "\n");
        }
    };

    setInterval(() => {
        renderDonut();
        A += A_spacing;
        B += B_spacing;
    }, 50);
}

donut();
