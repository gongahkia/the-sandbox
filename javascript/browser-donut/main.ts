const canvas = document.getElementById('donutCanvas') as HTMLCanvasElement;
const ctx = canvas.getContext('2d')!;

canvas.width = window.innerWidth;
canvas.height = window.innerHeight;

const radius1 = 80;
const radius2 = 150;

const K1 = 200;
const K2 = 5;
const A_spacing = 0.07;
const B_spacing = 0.03;

let A = 1.0;  // Changed to let
let B = 1.0;  // Changed to let

const fps = 30;
const interval = 1000 / fps;

let lastTime = 0;

function drawDonut(A: number, B: number) {
    ctx.clearRect(0, 0, canvas.width, canvas.height);

    const sinA = Math.sin(A);
    const cosA = Math.cos(A);
    const sinB = Math.sin(B);
    const cosB = Math.cos(B);

    for (let theta = 0; theta < 2 * Math.PI; theta += A_spacing) {
        const sinTheta = Math.sin(theta);
        const cosTheta = Math.cos(theta);

        for (let phi = 0; phi < 2 * Math.PI; phi += B_spacing) {
            const sinPhi = Math.sin(phi);
            const cosPhi = Math.cos(phi);

            const circleX = radius2 + radius1 * cosTheta;
            const circleY = radius1 * sinTheta;

            const x = circleX * (cosB * cosPhi + sinA * sinB * sinPhi) - circleY * cosA * sinB;
            const y = circleX * (sinB * cosPhi - sinA * cosB * sinPhi) + circleY * cosA * cosB;
            const z = K2 + cosA * circleX * sinPhi + circleY * sinA;
            const ooz = 1 / z;

            const xp = (canvas.width / 2) + K1 * ooz * x;
            const yp = (canvas.height / 2) - K1 * ooz * y;

            const luminance = Math.max(0, cosPhi * cosTheta * sinB - cosA * cosTheta * sinPhi - sinA * sinTheta + cosB * (cosA * sinTheta - cosTheta * sinA * sinPhi));

            ctx.fillStyle = `rgba(255, 255, 255, ${luminance})`;
            ctx.fillRect(xp, yp, 2, 2);
        }
    }
}

function animate(time: number) {
    const elapsedTime = time - lastTime;

    if (elapsedTime >= interval) {
        drawDonut(A, B);
        A += 0.07;  // A and B are now mutable
        B += 0.03;  // A and B are now mutable
        lastTime = time;
    }

    requestAnimationFrame(animate);
}

animate(0);
