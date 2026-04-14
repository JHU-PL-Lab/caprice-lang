# Caprice LSP

A language server for the Caprice language, providing inline diagnostics in VS Code.

## Usage

Install the extension and open any `.caprice` file — type checking runs automatically.

## Typechecker binary

The extension expects `caprice_typecheck_lsp.exe` in your workspace root. If the binary is not found, the extension will show an error and type checking will be disabled.

## Tips

- **Toggle type checking** — click the status bar item in the lower-right corner (`✓ Caprice Typecheck` / `⊘ Caprice Typecheck`) to enable or disable type checking without reloading the window.
- **Diagnostics** — errors appear as red underlines inline. Hover over them for details.
- **Output log** — open the Output panel (`View → Output`) and select **Caprice Language Server** to see the typechecker's raw output, useful for debugging.
