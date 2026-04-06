import {
	createConnection,
	DidChangeTextDocumentParams,
	ProposedFeatures,
	InitializeParams,
	InitializeResult,
	TextDocumentSyncKind
} from 'vscode-languageserver/node';
import { TextDocument } from 'vscode-languageserver-textdocument';
import { TextDocumentContentChangeEvent } from 'vscode-languageserver/node';
import { spawn, ChildProcessWithoutNullStreams } from 'child_process';
import * as path from 'path';
import { fileURLToPath } from 'url';
import * as fs from 'fs';

import type { CheckerPacket } from './protocol';
import type { Range } from 'vscode-languageserver-types';
import { parseLine } from './protocol';
import { DiagnosticsManager } from './diagnostics';

const connection = createConnection(ProposedFeatures.all);
const docs = new Map<string, TextDocument>();
const diagnostics = new DiagnosticsManager(connection);

let typecheckerPath = '';
let ocamlChecker: ChildProcessWithoutNullStreams;
let buffer = '';
let currentUri = '';
let checkerBusy = false;

function startChecker(): ChildProcessWithoutNullStreams {
	const checker = spawn(typecheckerPath);

	checker.stdout.on('data', (data) => {
		console.log(data.toString());

		buffer += data.toString();
		const lines = buffer.split('\n');
		buffer = lines.pop()!;
		for (const line of lines) {
			const msg = parseLine(line);
			if (!msg) {
				connection.console.warn(`unparsed: ${line}`);
				continue;
			}
			if (msg.tag === 'done') {
				checkerBusy = false;
				continue;
			}
			diagnostics.handle(currentUri, msg);
		}
	});

	checker.stderr.on('data', (data) => {
		connection.console.error(`OCaml error: ${data}`);
	});

	return checker;
}

function restartChecker(): void {
	ocamlChecker.stdout.removeAllListeners('data');
	ocamlChecker.stderr.removeAllListeners('data');
	ocamlChecker.kill();
	buffer = '';
	checkerBusy = false;
	diagnostics.cancelPendingTimers(currentUri);
	ocamlChecker = startChecker();
}

process.on('exit', () => ocamlChecker?.kill());

function updateDocument(params: DidChangeTextDocumentParams): TextDocument | undefined {
	const doc = docs.get(params.textDocument.uri);
	if (!doc) {
		connection.console.warn(`document not found: ${params.textDocument.uri}`);
		return undefined;
	}

	const updated = TextDocument.update(doc, params.contentChanges, params.textDocument.version);
	docs.set(params.textDocument.uri, updated);
	return updated;
}

function writeFramedMessage(message: CheckerPacket): void {
	const body = JSON.stringify(message);
	const len = Buffer.byteLength(body, 'utf8');
	ocamlChecker.stdin.write(`${len}\n${body}`, 'utf8');
	checkerBusy = true;
}

connection.onInitialize((params: InitializeParams) => {
	const rootUri = params.workspaceFolders?.[0]?.uri;
	const bundledPath = path.join(__dirname, '..', '..', 'typecheck_lsp.exe');
	if (rootUri) {
		const workspacePath = path.join(fileURLToPath(rootUri), 'typecheck_lsp.exe');
		typecheckerPath = fs.existsSync(workspacePath) ? workspacePath : bundledPath;
	} else {
		typecheckerPath = bundledPath;
	}
	ocamlChecker = startChecker();

	const result: InitializeResult = {
		capabilities: {
			textDocumentSync: TextDocumentSyncKind.Incremental
		}
	};
	return result;
});

function sendPacket(doc: TextDocument, changes: Range[]): void {
	try {
		if (checkerBusy) restartChecker();
		diagnostics.onNewCheck(doc.uri, doc.uri !== currentUri, changes);
		currentUri = doc.uri;
		writeFramedMessage({
			uri: doc.uri,
			version: doc.version,
			fullText: doc.getText(),
			changes
		});
	} catch (error) {
		connection.console.error(`protocol_error:${String(error)}`);
	}
}

function extractChanges(params: DidChangeTextDocumentParams): Range[] {
	const incrementalChanges = params.contentChanges.filter(
		TextDocumentContentChangeEvent.isIncremental
	);

	if (incrementalChanges.length !== params.contentChanges.length) {
		throw new Error(
			`Expected incremental changes for ${params.textDocument.uri}@${params.textDocument.version}`
		);
	}

	return incrementalChanges.map((change) => change.range);
}

connection.onDidOpenTextDocument(({ textDocument }) => {
	const doc = TextDocument.create(
		textDocument.uri,
		textDocument.languageId,
		textDocument.version,
		textDocument.text
	);
	docs.set(textDocument.uri, doc);

	sendPacket(doc, [{ start: { line: 0, character: 0 }, end: { line: 0, character: 0 } }]);
});

connection.onDidChangeTextDocument((params) => {
	const updated = updateDocument(params);
	if (!updated) return;

	sendPacket(updated, extractChanges(params));
});

connection.onDidCloseTextDocument(({ textDocument }) => {
	docs.delete(textDocument.uri);
});

connection.listen();
