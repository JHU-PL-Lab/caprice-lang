import type { Range } from 'vscode-languageserver-types';

// TypeScript -> OCaml
export type CheckerPacket = {
	uri: string;
	version: number;
	fullText: string;
	changes: Range[];
};

// OCaml -> TypeScript
export type OcamlMessage =
  | { tag: 'pending';          range: Range }
  | { tag: 'ok';               range: Range }
  | { tag: 'error';            range: Range; msg: string }
  | { tag: 'timeout';          range: Range }
  | { tag: 'unknown';          range: Range }
  | { tag: 'exhausted_pruned'; range: Range }
  | { tag: 'done' }
  | { tag: 'parse_error';      line: number; col: number; tok: string }
  | { tag: 'splay_error';      range: Range; msg: string }
  | { tag: 'refinement_warning'; range: Range }

function parseRange(parts: string[]) {
  return {
    range: {
      start: { line: +parts[1], character: +parts[2] },
      end:   { line: +parts[3], character: +parts[4] },
    } satisfies Range,
  };
}

export function parseLine(line: string): OcamlMessage | null {
  const parts = line.split(':');
  switch (parts[0]) {
    case 'pending': return { tag: 'pending', ...parseRange(parts) };
    case 'ok': return { tag: 'ok', ...parseRange(parts) };
    case 'error': return { tag: 'error', ...parseRange(parts), msg: parts.slice(5).join(':') };
    case 'parse_error': return { tag: 'parse_error', line: +parts[1], col: +parts[2], tok: parts[3] };
    case 'timeout': return { tag: 'timeout', ...parseRange(parts) };
    case 'unknown': return { tag: 'unknown', ...parseRange(parts) };
    case 'exhausted_pruned': return { tag: 'exhausted_pruned', ...parseRange(parts) };
    case 'done': return { tag: 'done' };
    case 'splay_error': return { tag: 'splay_error', ...parseRange(parts), msg: parts.slice(5).join(':') };
    case 'refinement_warning': return { tag: 'refinement_warning', ...parseRange(parts) };
    default: return null;
  }
}
