export interface Position {
  line: number;
  character: number;
}

export interface Range {
  start: Position;
  end: Position;
}

export interface Diagnostic {
  range: Range;
  message: string;
  severity: number;
}

export interface CompletionItem {
  label: string;
  kind?: number;
  detail?: string;
}