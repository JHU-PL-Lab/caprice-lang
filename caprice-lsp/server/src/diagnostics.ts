import {
  Diagnostic,
  DiagnosticSeverity,
  Connection,
} from 'vscode-languageserver/node';
import type { OcamlMessage } from './protocol';
import type { Range } from 'vscode-languageserver-types';

function rangeKey(r: Range): string {
  return `${r.start.line}:${r.start.character}:${r.end.line}:${r.end.character}`;
}

function parseErrorDiagnostic(msg: Extract<OcamlMessage, { tag: 'parse_error' }>): Diagnostic {
  const [start, end] = msg.tok.length > 0
    ? [msg.col - msg.tok.length, msg.col]
    : [msg.col, msg.col + 1];
  return {
    range: {
      start: { line: msg.line - 1, character: start },
      end:   { line: msg.line - 1, character: end },
    },
    message: msg.tok.length > 0 ? `Parse error: unexpected '${msg.tok}'` : 'Parse error',
    severity: DiagnosticSeverity.Error,
  };
}

function toSeverity(tag: OcamlMessage['tag']): DiagnosticSeverity | null {
  switch (tag) {
    case 'error':               return DiagnosticSeverity.Error;
    case 'timeout':
    case 'unknown':
    case 'exhausted_pruned':    return DiagnosticSeverity.Warning;
    default:                    return null;
  }
}

export class DiagnosticsManager {
  private byStmt = new Map<string, Diagnostic>();
  private pendingParseError: { diagnostic: Diagnostic; timer: NodeJS.Timeout } | null = null;
  private inFlight = new Map<string, { range: Range; timer: NodeJS.Timeout }>();
  private editLine = 0;

  constructor(private connection: Connection) {}

  private flush(uri: string): void {
    this.connection.sendDiagnostics({
      uri,
      diagnostics: Array.from(this.byStmt.values()),
    });
  }

  private commitPending(uri: string): void {
    if (!this.pendingParseError) return;
    const { diagnostic } = this.pendingParseError;
    this.pendingParseError = null;
    this.byStmt.set(rangeKey(diagnostic.range), diagnostic);
    this.flush(uri);
  }

  private invalidate(key: string, range: Range): void {
    this.byStmt.delete(key);
    const entry = this.inFlight.get(key);
    if (entry !== undefined) { clearTimeout(entry.timer); this.inFlight.delete(key); }
    if (this.pendingParseError !== null &&
        this.pendingParseError.diagnostic.range.start.line >= range.start.line) {
      clearTimeout(this.pendingParseError.timer);
      this.pendingParseError = null;
    }
  }

  handle(uri: string, msg: OcamlMessage): void {
    switch (msg.tag) {
      case 'pending': {
        const key = rangeKey(msg.range);
        const existing = this.inFlight.get(key);
        if (existing !== undefined) clearTimeout(existing.timer);
        this.inFlight.set(key, {
          range: msg.range,
          timer: setTimeout(() => {
            this.byStmt.set(key, {
              range: msg.range,
              message: 'checking...',
              severity: DiagnosticSeverity.Warning,
            });
            this.flush(uri);
          }, 250),
        });
        break;
      }
      case 'parse_error': {
        const diagnostic = parseErrorDiagnostic(msg);
        if (this.pendingParseError !== null) {
          clearTimeout(this.pendingParseError.timer);
        }
        for (const [key, diag] of this.byStmt) {
          if (diag.range.end.line >= this.editLine) this.byStmt.delete(key);
        }
        this.flush(uri);
        this.pendingParseError = {
          diagnostic,
          timer: setTimeout(() => { this.commitPending(uri); }, 1000),
        };
        break;
      }

      case 'error':
      case 'timeout':
      case 'unknown':
      case 'exhausted_pruned': {
        const key = rangeKey(msg.range);
        this.invalidate(key, msg.range);
        if (msg.tag === 'error') this.byStmt.delete(key + ':splay');
        const severity = toSeverity(msg.tag)!;
        const diagnostic: Diagnostic = {
          range: msg.range,
          message: msg.tag === 'error' ? msg.msg : msg.tag,
          severity,
        };

        if (msg.tag === 'error' && msg.msg.includes('Unbound variable')) {
          for (const [k, diag] of this.byStmt) {
            if (diag.range.start.line >= msg.range.start.line) this.byStmt.delete(k);
          }
        }
        this.byStmt.set(key, diagnostic);
        this.flush(uri);
        break;
      }

      case 'splay_error': {
        const key = rangeKey(msg.range);
        this.byStmt.set(key + ':splay', {
          range: msg.range,
          message: msg.msg,
          severity: DiagnosticSeverity.Warning,
        });
        this.flush(uri);
        break;
      }

      case 'ok': {
        const key = rangeKey(msg.range);
        this.invalidate(key, msg.range);
        this.byStmt.delete(key + ':splay');
        this.flush(uri);
        break;
      }
    }
  }

  onNewCheck(uri: string, isNewDoc: boolean, changes: Range[]): void {
    if (this.pendingParseError !== null) {
      clearTimeout(this.pendingParseError.timer);
      this.pendingParseError = null;
    }
    if (isNewDoc) {
      this.byStmt.clear();
      this.editLine = 0;
    } else {
      this.editLine = Math.min(...changes.map(c => c.start.line));
      for (const [key, diag] of this.byStmt) {
        if (diag.range.end.line >= this.editLine) this.byStmt.delete(key);
      }
      this.flush(uri);
    }
  }

  cancelPendingTimers(uri: string): void {
    if (this.pendingParseError !== null) {
      clearTimeout(this.pendingParseError.timer);
      this.pendingParseError = null;
    }
    for (const [key, { range, timer }] of this.inFlight) {
      clearTimeout(timer);
      this.byStmt.set(key, {
        range,
        message: 'timeout',
        severity: DiagnosticSeverity.Warning,
      });
    }
    this.inFlight.clear();
    this.flush(uri);
  }
}
