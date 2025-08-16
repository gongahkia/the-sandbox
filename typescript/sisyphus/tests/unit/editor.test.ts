import React from 'react';
import { render, fireEvent } from '@testing-library/react';
import Editor from '@sisyphus/core-editor/src/components/Editor';

describe('Editor', () => {
  it('renders initial value and propagates changes', () => {
    let editedText = '';
    const { getByRole } = render(
      <Editor value="hello world" onChange={(val) => (editedText = val)} />
    );
    // Simulate text entry (simplified)
    fireEvent.input(getByRole('textbox'), { target: { value: 'changed!' } });
    expect(editedText).toBe('changed!');
  });
});
