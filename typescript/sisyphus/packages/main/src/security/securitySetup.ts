import { session } from 'electron';

// Context isolation, disable Node integration is set in BrowserWindow webPreferences.
// Here, set up CSP & restrict navigation, external links, and enable safe features.

export function applySecurityPolicies() {
  // Restrict navigation
  session.defaultSession.webRequest.onBeforeRequest({ urls: ['*://*/*'] }, (details, callback) => {
    if (!details.url.startsWith('file://') && !details.url.startsWith('https://trusted-here')) {
      return callback({ cancel: true });
    }
    callback({});
  });

  // Set Content Security Policy
  session.defaultSession.webRequest.onHeadersReceived((details, callback) => {
    const csp = "default-src 'self'; script-src 'self'; style-src 'self'; img-src 'self' data:;";
    if (details.responseHeaders) {
      details.responseHeaders['Content-Security-Policy'] = [csp];
    }
    callback({ responseHeaders: details.responseHeaders });
  });

  // Block insecure features, enable sandbox if needed.
  // Electron >=12 is secure by default, but double-check!
}