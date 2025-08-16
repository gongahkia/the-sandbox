import { exec } from 'node:child_process';

console.log("Starting renderer (Vite)...");

const viteProc = exec("pnpm --filter @sisyphus/renderer dev");
viteProc.stdout.pipe(process.stdout);
viteProc.stderr.pipe(process.stderr);

console.log("Starting Electron main process...");
const electronProc = exec("pnpm --filter @sisyphus/main start");
electronProc.stdout.pipe(process.stdout);
electronProc.stderr.pipe(process.stderr);
