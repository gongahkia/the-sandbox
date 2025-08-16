export async function loadPluginModule(entry: string): Promise<any> {
  return await import(/* @vite-ignore */ entry);
}
