const textInput = document.getElementById('textInput');
const verboseToggle = document.getElementById('verboseToggle');
const playButton = document.getElementById('playButton');
const modeDisplay = document.getElementById('modeDisplay');
const processedText = document.getElementById('processedText');

let isPlaying = false;

const fillerWords = [
    'you know, ',
    'like, ',
    'basically, ',
    'I mean, ',
    'actually, ',
    'sort of, ',
    'kind of, ',
    'um, ',
    'well, ',
    'essentially, '
];

function addFillerWords(text) {
    return text.split('. ').map(sentence => {
        if (sentence.trim().length === 0) return sentence;
        const numFillers = Math.floor(Math.random() * 3) + 1;
        let modifiedSentence = sentence;
        
        for (let i = 0; i < numFillers; i++) {
            const position = Math.floor(Math.random() * modifiedSentence.length);
            const filler = fillerWords[Math.floor(Math.random() * fillerWords.length)];
            modifiedSentence = modifiedSentence.slice(0, position) + filler + modifiedSentence.slice(position);
        }
        
        return modifiedSentence;
    }).join('. ');
}

function updateProcessedText() {
    const originalText = textInput.value;
    const isVerbose = verboseToggle.checked;
    
    const processed = isVerbose ? addFillerWords(originalText) : originalText;
    processedText.textContent = processed || 'Processed text will appear here...';
    playButton.disabled = !originalText;
    
    modeDisplay.textContent = isVerbose ? 'Verbose mode: ON' : 'Concise mode: ON';
}

function updatePlayButton() {
    const playIcon = `<svg class="play-icon" viewBox="0 0 24 24" width="24" height="24">
        ${isPlaying ? '<rect x="4" y="4" width="16" height="16" />' : '<polygon points="5,3 19,12 5,21" />'}
    </svg>`;
    
    playButton.innerHTML = playIcon + `<span>${isPlaying ? 'Stop' : 'Play'}</span>`;
}

function handlePlayClick() {
    if (!('speechSynthesis' in window)) {
        alert('Speech synthesis is not supported in your browser.');
        return;
    }

    if (isPlaying) {
        window.speechSynthesis.cancel();
        isPlaying = false;
    } else {
        const utterance = new SpeechSynthesisUtterance(processedText.textContent);
        utterance.onend = () => {
            isPlaying = false;
            updatePlayButton();
        };
        window.speechSynthesis.speak(utterance);
        isPlaying = true;
    }
    
    updatePlayButton();
}

textInput.addEventListener('input', updateProcessedText);
verboseToggle.addEventListener('change', updateProcessedText);
playButton.addEventListener('click', handlePlayClick);
updateProcessedText();
updatePlayButton();