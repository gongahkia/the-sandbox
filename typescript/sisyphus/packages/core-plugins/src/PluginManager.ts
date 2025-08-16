import { PluginMeta } from '@sisyphus/shared/src/types/Plugin';

export class PluginManager {
  private plugins: Map<string, PluginMeta> = new Map();

  load(meta: PluginMeta) {
    this.plugins.set(meta.id, meta);
    if (meta.enabled) {
      this.initialize(meta);
    }
  }

  initialize(meta: PluginMeta) {
    // Dynamically import plugin entry module, register commands/UI as needed
    import(/* @vite-ignore */ meta.entry).then(module => {
      if (typeof module.init === 'function') module.init();
    });
  }

  enable(id: string) {
    const plugin = this.plugins.get(id);
    if (plugin) {
      plugin.enabled = true;
      this.initialize(plugin);
    }
  }

  disable(id: string) {
    const plugin = this.plugins.get(id);
    if (plugin) plugin.enabled = false;
    // Call plugin unload lifecycle if it exists
  }

  getAll() {
    return Array.from(this.plugins.values());
  }
}
