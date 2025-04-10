chrome.runtime.onInstalled.addListener(function() {
  console.log("Spotify Playlist Editor extension installed");
});

chrome.tabs.onUpdated.addListener((tabId, changeInfo, tab) => {
  if (changeInfo.status === 'complete' && tab.url && tab.url.match(/https:\/\/open\.spotify\.com\/playlist\/[a-zA-Z0-9]+/)) {
    chrome.tabs.sendMessage(tabId, { action: "activateExtension" });
  }
});