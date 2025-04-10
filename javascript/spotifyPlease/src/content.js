console.log("Spotify Playlist Editor content script loaded");

chrome.runtime.onMessage.addListener((message, sender, sendResponse) => {
  if (message.action === "activateExtension") {
    console.log("Extension activated on Spotify playlist page");
  }
});

function scrollToBottom() {
  const scrollbar = document.querySelector('div.main-view-container__scroll-node-child div.os-scrollbar-vertical div.os-scrollbar-handle');
  if (scrollbar) {
    console.log("Found scrollbar, scrolling to bottom");
    const scrollContainer = document.querySelector('div.main-view-container__scroll-node-child');
    if (scrollContainer) {
      scrollContainer.scrollTop = scrollContainer.scrollHeight;
      scrollbar.style.top = "100%";
      console.log("Scrolled to bottom, waiting 30 seconds before extracting playlist items");
      setTimeout(extractPlaylistItems, 30000);
    } else {
      console.log("Scrollable container not found");
    }
  } else {
    console.log("Scrollbar not found, retrying in 2 seconds");
    setTimeout(scrollToBottom, 2000);
  }
}

function extractPlaylistItems() {
  console.log("Extracting playlist items");
  const contentSpacingDivs = document.querySelectorAll('div.main-view-container__scroll-node-child div.contentSpacing');
  let targetDiv = null;
  for (const div of contentSpacingDivs) {
    if (div.classList.length === 1 && div.classList.contains('contentSpacing')) {
      targetDiv = div;
      break;
    }
  }
  if (targetDiv) {
    console.log("Found content spacing div");
    const rows = targetDiv.querySelectorAll('div[role="row"][aria-rowindex]');
    console.log(`Found ${rows.length} rows`);
    rows.forEach(row => {
      const rowIndex = parseInt(row.getAttribute('aria-rowindex'));
      
      if (rowIndex >= 2) {
        const innerText = row.innerText;
        console.log(`Row ${rowIndex}: ${innerText}`);
      }
    });
  } else {
    console.log("Content spacing div not found");
  }
}

window.addEventListener('load', () => {
  console.log("Page loaded, starting extension functionality");
  setTimeout(scrollToBottom, 3000);
});