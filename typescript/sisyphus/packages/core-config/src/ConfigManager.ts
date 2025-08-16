import { AppConfig } from '@sisyphus/shared/src/types/AppConfig';
import { validateConfig, deepMerge } from '@sisyphus/shared/src/utils/validateConfig';

export class ConfigManager {
  private config: AppConfig;

  constructor(defaults: AppConfig) {
    this.config = defaults;
  }

  load(userConfig: Partial<AppConfig>) {
    this.config = deepMerge(this.config, userConfig);
    validateConfig(this.config);
  }

  get<T = unknown>(key: keyof AppConfig): T {
    return this.config[key] as T;
  }

  set(key: keyof AppConfig, value: any) {
    this.config[key] = value;
    // could persist using IPC to main/fs
  }

  getAll() {
    return { ...this.config };
  }
}