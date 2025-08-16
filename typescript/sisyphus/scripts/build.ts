import { execSync } from 'node:child_process';

const packages = [
  'main', 'renderer', 'shared', 'core-editor', 'core-terminal', 'core-explorer',
  'core-lsp', 'core-config', 'core-plugins', 'core-themes', 'plugin-examples'
];

for (const pkg of packages) {
  console.log(`Building ${pkg}...`);
  execSync(`pnpm --filter @sisyphus/${pkg} run build`, { stdio: 'inherit' });
}
console.log('All packages built.');
