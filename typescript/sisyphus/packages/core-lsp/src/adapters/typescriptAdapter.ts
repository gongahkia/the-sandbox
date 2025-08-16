import { LSPAdapterMeta } from '@sisyphus/shared/src/types/LSP';

export const typescriptAdapter: LSPAdapterMeta = {
  language: 'typescript',
  serverPath: 'node_modules/.bin/typescript-language-server',
  args: ['--stdio'],
  enabled: true,
};