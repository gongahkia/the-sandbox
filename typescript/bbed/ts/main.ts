let fileName = {name:String};

const loadFileButton = document.getElementById("loadFile");
loadFileButton?.addEventListener("change", loadFile);

let fReader = new FileReader();

fReader.onload = function (event:any) {
    const textArea = <HTMLInputElement>document.getElementById("localTextArea")!;
    textArea.value = event.target.result;
};

function loadFile(event:any) {
    const fileList = event.target.files;
    console.log(fileList[0]);
    fileName.name = fileList[0].name; // --- returns file's file name

    const entryContent = document.getElementById("entryScreen")!;
    entryContent.remove();

    const overallContent = document.getElementById("overallDiv")!; 
    overallContent.innerHTML += "<div class='mainScreen'><textarea id='localTextArea' class='localTextArea' rows='35' cols='175' spellcheck='false'></textarea><button id='saveFileButton' type='button'>Save a copy</button></div>";

    const saveFileButton = document.getElementById("saveFileButton");
    saveFileButton?.addEventListener("click", saveFile);

    fReader.readAsText(fileList[0]);
}

function saveFile(event:any) {
    const textArea = <HTMLInputElement>document.getElementById("localTextArea")!;
    let finalText = textArea.value;
    finalText = finalText.replace(/\n/g, "\r\n"); // To retain the Line breaks.
    let blob = new Blob([finalText], { type: "text/plain"});
    let anchor = document.createElement("a");
    anchor.download = `${fileName.name}`;
    anchor.href = window.URL.createObjectURL(blob);
    anchor.target ="_blank";
    anchor.style.display = "none"; // just to be safe!
    document.body.appendChild(anchor);
    anchor.click();
    document.body.removeChild(anchor);
}
