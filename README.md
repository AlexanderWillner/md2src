# Mardown to Source

Simple rust binary to extract code blocks marked with triple backticks from markdown files into source files.

[![Build Status](https://github.com/alexanderwillner/md2src/workflows/Build-Test/badge.svg)](https://github.com/AlexanderWillner/md2src/actions) [![Coverage Status](https://coveralls.io/repos/github/AlexanderWillner/md2src/badge.svg?branch=master)](https://coveralls.io/github/AlexanderWillner/md2src?branch=master) [![Crates.io](https://img.shields.io/crates/d/md2src)](https://crates.io/crates/md2src)

## Installation

To download the latest release, please run ```cargo install md2src```.

## Example

Run run ```md2src``` to create the source file named ```code_snippet_000.rs``` from this code:

```rust
fn main() {
    todo!();
}
```

## Help

```bash
$ md2src --help
md2src 1.0.0
Alexander Willner <alex@willner.ws>
Markdown to source. Extracts code blocks marked with triple backticks into files.

USAGE:
    md2src [OPTIONS] [ARGS]

FLAGS:
    -h, --help       Prints help information
    -V, --version    Prints version information

OPTIONS:
    -e, --extension <extension>    File extension for code files [default: rs]
    -l, --language <language>      Code snippet language to extract [default: rust]
    -p, --prefix <prefix>          Prefix code files with this string [default: code_snippet_]
    -i, --ignore <string>          Ignore code with this string [default: #[doc = "This will fail]

ARGS:
    <filename>    Markdown file that contains the code snippets [default: README.md]
    <folder>      Folder for the code snippets [default: .]
```
