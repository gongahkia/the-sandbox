export interface PluginMeta {
  id: string;
  name: string;
  version: string;
  author: string;
  enabled: boolean;
  entry: string;
  description?: string;
}