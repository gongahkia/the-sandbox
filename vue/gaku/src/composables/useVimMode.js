import { ref, reactive } from 'vue';

export function useVimMode() {
  const isVimMode = ref(true);
  const vimState = reactive({
    mode: 'normal', // normal, insert, visual
    buffer: '',
    visualStart: null,
    visualEnd: null,
    registers: {
      '"': '' // default register
    }
  });

  function handleKeydown(event, editor) {
    if (!isVimMode.value) return true;
    
    if (vimState.mode === 'normal') {
      return handleNormalMode(event, editor);
    } else if (vimState.mode === 'insert') {
      return handleInsertMode(event, editor);
    } else if (vimState.mode === 'visual') {
      return handleVisualMode(event, editor);
    }
    
    return true;
  }

  function handleNormalMode(event, editor) {
    const key = event.key;
    
    // Movement keys
    if (key === 'h') {
      moveCursor(editor, 'left');
      return false;
    } else if (key === 'j') {
      moveCursor(editor, 'down');
      return false;
    } else if (key === 'k') {
      moveCursor(editor, 'up');
      return false;
    } else if (key === 'l') {
      moveCursor(editor, 'right');
      return false;
    } else if (key === '0') {
      moveCursorToLineStart(editor);
      return false;
    } else if (key === '$') {
      moveCursorToLineEnd(editor);
      return false;
    }
    
    // Mode switching
    if (key === 'i') {
      vimState.mode = 'insert';
      return false;
    } else if (key === 'a') {
      vimState.mode = 'insert';
      moveCursor(editor, 'right');
      return false;
    } else if (key === 'v') {
      vimState.mode = 'visual';
      vimState.visualStart = getCursorPosition(editor);
      vimState.visualEnd = vimState.visualStart;
      return false;
    }
    
    // Deletion
    if (key === 'x') {
      deleteCharAtCursor(editor);
      return false;
    } else if (key === 'd' && event.repeat === false) {
      vimState.buffer = 'd';
      return false;
    }
    
    // Handle buffer commands
    if (vimState.buffer === 'd') {
      if (key === 'd') {
        deleteLine(editor);
        vimState.buffer = '';
        return false;
      } else if (key === 'w') {
        deleteWord(editor);
        vimState.buffer = '';
        return false;
      }
    }
    
    return true;
  }

  function handleInsertMode(event, editor) {
    if (event.key === 'Escape') {
      vimState.mode = 'normal';
      editor.focus();
      return false;
    }
    return true;
  }

  function handleVisualMode(event, editor) {
    if (event.key === 'Escape') {
      vimState.mode = 'normal';
      clearSelection(editor);
      return false;
    }
    
    // Movement in visual mode extends selection
    if (['h', 'j', 'k', 'l', '0', '$'].includes(event.key)) {
      const result = handleNormalMode(event, editor);
      vimState.visualEnd = getCursorPosition(editor);
      updateSelection(editor);
      return result;
    }
    
    // Deletion in visual mode
    if (event.key === 'd' || event.key === 'x') {
      deleteSelection(editor);
      vimState.mode = 'normal';
      return false;
    }
    
    return true;
  }

  // Helper functions for cursor movement and text manipulation
  function moveCursor(editor, direction) {
    // Implement cursor movement based on editor API
    console.log(`Move cursor ${direction}`);
  }

  function moveCursorToLineStart(editor) {
    editor.setCursorPosition({ line: editor.getCursorPosition().line, column: 0 });
    console.log('Move to line start');
  }

  function moveCursorToLineEnd(editor) {
    const line = editor.getCursorPosition().line;
    const lineLength = editor.getLine(line).length;
    editor.setCursorPosition({ line, column: lineLength });
    console.log('Move to line end');
  }

  function getCursorPosition(editor) {
    return editor.getCursorPosition();
  }

  function deleteCharAtCursor(editor) {
    const pos = editor.getCursorPosition();
    editor.remove({ line: pos.line, column: pos.column }, { line: pos.line, column: pos.column + 1 });
    console.log('Delete char at cursor');
  }

  function deleteLine(editor) {
    const line = editor.getCursorPosition().line;
    editor.removeLine(line);
    console.log('Delete line');
  }
  
  function deleteWord(editor) {
    const pos = editor.getCursorPosition();
    const lineText = editor.getLine(pos.line);
    const wordEnd = lineText.indexOf(' ', pos.column);
    editor.remove(pos, { line: pos.line, column: wordEnd === -1 ? lineText.length : wordEnd });
    console.log('Delete word');
  }
  
  function clearSelection(editor) {
    editor.clearSelection();
    console.log('Clear selection');
  }
  
  function updateSelection(editor) {
    editor.setSelection(vimState.visualStart, vimState.visualEnd);
    console.log('Update selection');
  }
  
  function deleteSelection(editor) {
    editor.removeSelection();
    console.log('Delete selection');
  }

  return {
    isVimMode,
    vimState,
    handleKeydown
  };
}