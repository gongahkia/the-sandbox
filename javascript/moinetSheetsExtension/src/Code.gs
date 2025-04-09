function onOpen() {
  var ui = SpreadsheetApp.getUi();
  ui.createMenu('Moinet time Logger')
    .addItem('Start timing', 'startTimer')
    .addItem('Stop timing', 'endTimer')
    .addToUi();
}

function startTimer() {
  var startTime = new Date();
  var sheet = SpreadsheetApp.getActiveSpreadsheet().getActiveSheet();  
  var nextRow = sheet.getLastRow() + 1;
  sheet.getRange(nextRow, 1).setValue(startTime);
  sheet.getRange(nextRow, 1).setNumberFormat("yyyy-mm-dd hh:mm:ss");
  sheet.getRange("D1").setValue(startTime);
  sheet.getRange(nextRow, 1).setFontFamily("Comic Sans MS").setFontSize(12);
  SpreadsheetApp.getUi().alert('Timer started at: ' + startTime);
}

function endTimer() {
  var sheet = SpreadsheetApp.getActiveSpreadsheet().getActiveSheet();
  var startTime = sheet.getRange("D1").getValue();
  
  if (!startTime) {
    SpreadsheetApp.getUi().alert('Please start the timer first.');
    return;
  }
  
  var endTime = new Date();  
  var nextRow = sheet.getLastRow();
  sheet.getRange(nextRow, 2).setValue(endTime);
  sheet.getRange(nextRow, 2).setNumberFormat("yyyy-mm-dd hh:mm:ss");
  
  var durationInMilliseconds = endTime - startTime;
  
  if (durationInMilliseconds < 0) {
    SpreadsheetApp.getUi().alert('End time cannot be earlier than start time.');
    return;
  }
  
  var totalSeconds = Math.floor(durationInMilliseconds / 1000);
  var hours = Math.floor(totalSeconds / 3600);
  var minutes = Math.floor((totalSeconds % 3600) / 60);
  var seconds = totalSeconds % 60;
  
  var durationFormatted = Utilities.formatString("%02d:%02d:%02d", hours, minutes, seconds);
  
  sheet.getRange(nextRow, 3).setValue(durationFormatted);
  
  sheet.getRange(nextRow, 2).setFontFamily("Comic Sans MS").setFontSize(12);
  sheet.getRange(nextRow, 3).setFontFamily("Comic Sans MS").setFontSize(12);
  
  SpreadsheetApp.getUi().alert('Timer ended at: ' + endTime + '\nTotal Duration: ' + durationFormatted);
}
