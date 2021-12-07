'use strict';
import * as vscode from 'vscode';
import { formatChunk } from './myluafmt';

const selectors: { language: string; scheme: string }[] = [
	{ language: 'lua', scheme: 'file' },
	{ language: 'lua', scheme: 'untitled' },
];

let diagnosticCollection: vscode.DiagnosticCollection;

export function activate(context: vscode.ExtensionContext) {
	diagnosticCollection = vscode.languages.createDiagnosticCollection('ttt-lua-format');
	context.subscriptions.push(vscode.languages.registerDocumentRangeFormattingEditProvider(selectors, new LuaFormatRangeProvider(context)));
	context.subscriptions.push(vscode.languages.registerDocumentFormattingEditProvider(selectors, new LuaFormatProvider(context)));
}

export class LuaFormatRangeProvider implements vscode.DocumentRangeFormattingEditProvider {
	private context: vscode.ExtensionContext;

	constructor(context: vscode.ExtensionContext) {
		this.context = context;
	}

	public provideDocumentRangeFormattingEdits(document: vscode.TextDocument, range: vscode.Range, options: vscode.FormattingOptions, token: vscode.CancellationToken): Thenable<vscode.TextEdit[]> {
		let data = document.getText();
		data = data.substring(document.offsetAt(range.start), document.offsetAt(range.end));
		return new Promise((resolve, reject) => {
			try {
				const resultStr = formatChunk(data);
				resolve([new vscode.TextEdit(range, resultStr)]);
			} catch (e: any) {
				console.log(e.toString());
				vscode.window.showInformationMessage(e.toString());
				reject(e);
			}
		});
	}
}

class LuaFormatProvider implements vscode.DocumentFormattingEditProvider {
	private context: vscode.ExtensionContext;

	constructor(context: vscode.ExtensionContext) {
		this.context = context;
	}

	public provideDocumentFormattingEdits(document: vscode.TextDocument, options: vscode.FormattingOptions, token: vscode.CancellationToken): Thenable<vscode.TextEdit[]> {
		const data = document.getText();
		return new Promise((resolve, reject) => {
			try {
				const resultStr = formatChunk(data);
				const range = document.validateRange(new vscode.Range(0, 0, Infinity, Infinity));
				resolve([new vscode.TextEdit(range, resultStr)]);
			} catch (e: any) {
				console.log(e.toString());
				vscode.window.showInformationMessage(e.toString());
				reject(e);
			}
		});
	}
}
