import * as path from 'path';
import {
	CodeLens,
	EventEmitter,
	ExtensionContext,
	Position,
	Range,
	StatusBarAlignment,
	StatusBarItem,
	commands,
	languages,
	window,
} from 'vscode';
import {
	LanguageClient,
	LanguageClientOptions,
	ServerOptions,
	TransportKind
} from 'vscode-languageclient/node';

type SplayMarker = {
	range: { start: { line: number; character: number }; end: { line: number; character: number } };
	message: string;
};

let client: LanguageClient;
let statusBar: StatusBarItem;
let enabled = true;

function updateStatusBar() {
	statusBar.text = enabled ? '$(check) Caprice Typecheck' : '$(circle-slash) Caprice Typecheck';
	statusBar.tooltip = enabled
		? 'Type checking ON — click to disable'
		: 'Type checking OFF — click to enable';
}

export function activate(context: ExtensionContext) {
	const serverModule = context.asAbsolutePath(
		path.join('server', 'out', 'server.js')
	);

	const serverOptions: ServerOptions = {
		run: { module: serverModule, transport: TransportKind.ipc },
		debug: { module: serverModule, transport: TransportKind.ipc }
	};

	const clientOptions: LanguageClientOptions = {
		documentSelector: [{ scheme: 'file', language: 'caprice' }]
	};

	client = new LanguageClient(
		'CapricelanguageServer',
		'Caprice Language Server',
		serverOptions,
		clientOptions
	);

	client.start();

	statusBar = window.createStatusBarItem(StatusBarAlignment.Right, 100);
	statusBar.command = 'caprice.toggleTypechecking';
	updateStatusBar();
	statusBar.show();

	const splayMarkers = new Map<string, SplayMarker[]>();
	const codeLensEmitter = new EventEmitter<void>();

	const toggle = commands.registerCommand('caprice.toggleTypechecking', async () => {
		if (enabled) {
			await client.stop();
			splayMarkers.clear();
			codeLensEmitter.fire();
		} else {
			await client.start();
		}
		enabled = !enabled;
		updateStatusBar();
	});

	const splayNotification = client.onNotification('caprice/splayMarkers', (params: { uri: string; markers: SplayMarker[] }) => {
		splayMarkers.set(params.uri, params.markers);
		codeLensEmitter.fire();
	});

	const showSplayError = commands.registerCommand('caprice.showSplayError', (message: string) => {
		window.showInformationMessage(message);
	});

	const codeLensProvider = languages.registerCodeLensProvider({ language: 'caprice' }, {
		onDidChangeCodeLenses: codeLensEmitter.event,
		provideCodeLenses(document) {
			return (splayMarkers.get(document.uri.toString()) ?? []).map(m => new CodeLens(
				new Range(
					new Position(m.range.start.line, 0),
					new Position(m.range.start.line, 0),
				),
				{
					title: m.message,
					command: 'caprice.showSplayError',
					arguments: [m.message],
				},
			));
		},
	});

	context.subscriptions.push(
		statusBar,
		toggle,
		showSplayError,
		codeLensProvider,
		codeLensEmitter,
		splayNotification,
	);
}

export function deactivate(): Thenable<void> | undefined {
	if (!client) {
		return undefined;
	}
	return client.stop();
}