const editor = document.getElementById('editor');
const validateBtn = document.getElementById('validateBtn');
const result = document.getElementById('result');

validateBtn.addEventListener('click', async () => {
  const code = editor.value;
  result.textContent = 'Validating...';

  try {
    const response = await window.electronAPI.validatePython(code);
    if (response.error === 0) {
      result.textContent = response.message;
    } else {
      result.textContent = `Syntax errors detected:\n${response.errors.map(err => `Line ${err.line}: ${err.detail}`).join('\n')}`;
    }
  } catch (error) {
    result.textContent = 'Error: Failed to validate code';
  }
});