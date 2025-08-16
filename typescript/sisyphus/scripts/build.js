"use strict";
Object.defineProperty(exports, "__esModule", { value: true });
var node_child_process_1 = require("node:child_process");
var packages = [
    'main', 'renderer', 'shared', 'core-editor', 'core-terminal', 'core-explorer',
    'core-lsp', 'core-config', 'core-plugins', 'core-themes', 'plugin-examples'
];
for (var _i = 0, packages_1 = packages; _i < packages_1.length; _i++) {
    var pkg = packages_1[_i];
    console.log("Building ".concat(pkg, "..."));
    (0, node_child_process_1.execSync)("pnpm --filter @sisyphus/".concat(pkg, " run build"), { stdio: 'inherit' });
}
console.log('All packages built.');
