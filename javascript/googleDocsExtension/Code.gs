function onOpen() {
  DocumentApp.getUi()
      .createMenu('Custom Menu')
      .addItem('Show Sidebar', 'showSidebar')
      .addToUi();
}

function showSidebar() {
  const html = HtmlService.createHtmlOutputFromFile('Sidebar')
      .setTitle('Gong testing the Sidebar');
  DocumentApp.getUi().showSidebar(html);
}

function insertText(text) {
  const body = DocumentApp.getActiveDocument().getBody();
  body.appendParagraph(text);
}
