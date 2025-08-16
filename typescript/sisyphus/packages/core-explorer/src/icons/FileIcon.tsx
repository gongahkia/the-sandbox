import React from 'react';

export function FileIcon({ type }: { type: string }) {
  if (type === 'folder')
    return <span role="img" aria-label="folder">📁</span>;
  if (type === 'js')
    return <span role="img" aria-label="js">🟨</span>;
  if (type === 'ts')
    return <span role="img" aria-label="ts">🔵</span>;
  return <span role="img" aria-label="file">📄</span>;
}