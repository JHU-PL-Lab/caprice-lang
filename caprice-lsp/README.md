# Caprice LSP

A language server for the Caprice language, providing inline diagnostics in VS Code.

## Installation

The following are a few very rough instructions to install the language server as a VS Code extension.

Prerequisite: you must have the Caprice language extension installed. See `../caprice-language-extension/README.md`.

1. Navigate to the `caprice-lsp` directory (the one this README is in).
2. Install the package dependencies and compile:
    ```
    npm install
    ```
3. (Begin from here to reinstall after upgrades) Then do
    ```
    vsce package --allow-missing-repository
    ```
    Answer `y`/`yes` to questions about missing licenses.
4. Run
    ```
    code --install-extension caprice-lsp-1.1.1.vsix
    ```
    or whatever version we're currently on (not just `1.1.1` every time).

To make changes and reinstall, only run the last two commands. Note you either need to
uninstall the previous version, or increment the version number.

Uninstall with

```sh
code --list-extensions
```

Note the package name, which is probably `undefined_publisher.caprice-lsp`, and then run

```sh
code --uninstall-extension undefined_publisher.caprice-lsp
```

Also uninstall from npm with

```sh
npm uninstall caprice-lsp@1.1.1
```

or whatever version(s) you have installed.


## Usage

Install the extension and open any `.caprice` file — type checking runs automatically.

## Typechecker binary

The extension expects `caprice_typecheck_lsp.exe` in your workspace root. If the binary is not found, the extension will show an error and type checking will be disabled. This binary can be built by running `dune build` from the `caprice-lang` directory (the root of this repository).

## Tips

- **Toggle type checking** — click the status bar item in the lower-right corner (`✓ Caprice Typecheck` / `⊘ Caprice Typecheck`) to enable or disable type checking without reloading the window.
- **Diagnostics** — errors appear as red underlines inline. Hover over them for details.
- **Output log** — open the Output panel (`View → Output`) and select **Caprice Language Server** to see the typechecker's raw output, useful for debugging the language server itself.
