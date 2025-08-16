import { execSync } from 'node:child_process';

try {
  console.log('Packaging Sisyphus IDE for release...');
  execSync('pnpm electron-builder', { stdio: 'inherit' });
  console.log('Release build complete!');
} catch (e) {
  console.error('Release failed:', e);
}
