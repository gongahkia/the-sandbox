"use strict";
Object.defineProperty(exports, "__esModule", { value: true });
var node_child_process_1 = require("node:child_process");
try {
    console.log('Packaging Sisyphus IDE for release...');
    (0, node_child_process_1.execSync)('pnpm electron-builder', { stdio: 'inherit' });
    console.log('Release build complete!');
}
catch (e) {
    console.error('Release failed:', e);
}
