import { AppConfig } from '../types/AppConfig';

/**
 * Validates app config object, returns valid or throws error.
 */
export function validateConfig(config: unknown): AppConfig {
  if (
    typeof config === 'object' &&
    config !== null &&
    'window' in config &&
    typeof (config as any).window.width === 'number' &&
    typeof (config as any).window.height === 'number'
  ) {
    return config as AppConfig;
  }
  throw new Error('Invalid config structure received');
}
