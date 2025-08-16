import { keymap } from '@codemirror/view';
import { defaultKeymap } from '@codemirror/commands';

export const defaultEditorKeymap = keymap.of([...defaultKeymap]);