# WGSL-Analyzer

> A VSCode extension providing validation syntax highlighting and formatting of WGSL (Web GPU Shading Language) files

*If you encounter any issues, please report them on [github](https://github.com/unfinishedprogram/wgsl-analyzer/issues)*

## Features

- ✅ **Syntax highlighting of WGSL files**
- ✅ **Code formatting**
- ✅ **Syntax validation**
- ✅ **Correctness validation**
- ✅ **Document outline**
- ✅ **Context aware auto-completion**
  - ✅ Local Variables
  - ✅ Global Constants
  - ✅ Functions
  - ✅ Keywords
  - ✅ Builtin Functions
  - ✅ Property Access
 - ✅ Experimental support for Rust-style `#include` lines when referenced files are open

## Planned Features

- 🚧 *Info on hover*
- 🚧 *Improved diagnostic messages*
- 🚧 *Goto definition*

## About

This extension is written in rust and uses Naga compiled to wasm to generate diagnostics.
This means that the extension should work on any platform, and does not require any external binaries.

## Developing

### Install pre-requisites
```sh
# Install wasm-pack from source
cargo install wasm-pack

# Install NPM deps 
npm i
```
