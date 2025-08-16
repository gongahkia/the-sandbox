import { LanguageClient, LanguageClientOptions, ServerOptions, TransportKind } from 'vscode-languageclient/node';
import { LSPAdapterMeta } from '@sisyphus/shared/src/types/LSP';

export class LSPClient {
  private clients: Record<string, LanguageClient> = {};

  constructor(private adapters: LSPAdapterMeta[]) {}

  startAll() {
    this.adapters.forEach(adapter => this.startAdapter(adapter));
  }

  startAdapter(adapter: LSPAdapterMeta) {
    const serverOptions: ServerOptions = {
      run: { command: adapter.serverPath, args: adapter.args || [], transport: TransportKind.ipc },
      debug: { command: adapter.serverPath, args: adapter.args || [], transport: TransportKind.ipc },
    };
    const clientOptions: LanguageClientOptions = {
      documentSelector: [{ scheme: 'file', language: adapter.language }],
      synchronize: { fileEvents: null },
    };
    const client = new LanguageClient(
      adapter.language,
      `${adapter.language} Language Server`,
      serverOptions,
      clientOptions
    );
    client.start();
    this.clients[adapter.language] = client;
  }

  sendNotification(language: string, method: string, params: any) {
    this.clients[language]?.sendNotification(method, params);
  }

  stopAll() {
    Object.values(this.clients).forEach(client => client.stop());
  }
}
