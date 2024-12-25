function createMonthlySheet() {
  var ss = SpreadsheetApp.getActiveSpreadsheet();
  var currentDate = new Date();
  var month = Utilities.formatDate(currentDate, Session.getScriptTimeZone(), "MMMM yyyy");
  if (ss.getSheetByName(month)) {
    Logger.log("Sheet for this month already exists.");
    return; 
  }
  var newSheet = ss.insertSheet(month);
  newSheet.getRange("A1:E100").setFontSize(12).setFontFamily("Comic Sans MS");
  newSheet.appendRow(["Date", "Description", "Income", "Expenses", "Net"]);
  for (var day = 1; day <= 31; day++) {
    var date = new Date(currentDate.getFullYear(), currentDate.getMonth(), day);
    if (date.getMonth() === currentDate.getMonth()) { 
      newSheet.appendRow([date, "", "", "", ""]); 
    }
  }
  var lastRow = newSheet.getLastRow();
  for (var i = 2; i <= lastRow; i++) {
    newSheet.getRange(i, 5).setFormula(`=C${i}-D${i}`);
  }
  newSheet.appendRow(["Total", "", `=SUM(C2:C${lastRow-1})`, `=SUM(D2:D${lastRow-1})`, `=SUM(E2:E${lastRow-1})`]);
  newSheet.getRange(lastRow + 1, 1, 1, 5).setFontSize(12).setFontFamily("Comic Sans MS");
  newSheet.setColumnWidth(2, newSheet.getColumnWidth(3) * 4);
  Logger.log("Monthly sheet created successfully.");
}