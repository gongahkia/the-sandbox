import path from 'node:path';
import { promises as fs } from 'node:fs';

const defaultConfig = {
  window: {
    width: 1280,
    height: 800,
  },
  devTools: process.env.NODE_ENV !== 'production',
  rendererUrl: undefined, // can override for dev
  // Add more defaults as needed
};

export async function loadMainConfig(): Promise<any> {
  const configPath = path.join(process.cwd(), 'config.json');
  try {
    const raw = await fs.readFile(configPath, 'utf-8');
    const loaded = JSON.parse(raw);
    return { ...defaultConfig, ...loaded };
  } catch (err) {
    // If file not found or error, fallback to default
    if (typeof (err as any)?.code === 'string' && (err as any).code !== 'ENOENT') {
      console.error('Config load error:', err);
    }
    return defaultConfig;
  }
}