// Copyright by Alexander Willner. See the LICENCE file for the license information.

//! Simple rust library and CLI to extract code blocks marked with triple backticks from markdown files into source files.
//!
//! [![Build Status](https://github.com/alexanderwillner/md2src/workflows/Build-Test/badge.svg)](https://github.com/AlexanderWillner/md2src/actions) [![Coverage Status](https://coveralls.io/repos/github/AlexanderWillner/md2src/badge.svg?branch=master)](https://coveralls.io/github/AlexanderWillner/md2src?branch=master) [![Crates.io](https://img.shields.io/crates/d/md2src?label=crate%20downloads)](https://crates.io/crates/md2src) [![download](https://img.shields.io/github/downloads/AlexanderWillner/md2src/total?label=binary%20downloads)](https://github.com/AlexanderWillner/md2src/releases)
//!
//! ## Installation
//!
//! To download the latest release, please run either ```cargo install md2src``` or ```brew install AlexanderWillner/tap/md2src```.
//!
//! ## Example
//!
//! Run run ```md2src README.md``` to create the source file named ```code_snippet_000.rs```.
//!

use exitfailure::ExitFailure;
use failure::ResultExt;
use pulldown_cmark::{CodeBlockKind, CowStr, Event, Options, Parser, Tag};
use std::fs;
use std::path::Path;

/// Markdown to source code
pub struct MD2Src {}

impl MD2Src {
    /// Returns a vector of extracted code snippets.
    ///
    /// # Arguments
    ///
    /// * `file`   - The markdown filename.
    /// * `lang`   - The language of the code snippets defined after three backticks
    /// * `ignore` - Ignore those code snippets that include this string
    ///
    pub fn get_snippets(
        self: &Self,
        file: String,
        lang: String,
        ignore: String,
    ) -> Result<Vec<String>, ExitFailure> {
        let markdown_input = fs::read_to_string(&file)?;
        let parser = Parser::new_ext(&markdown_input, Options::empty());
        let mut active = false;
        let mut result: Vec<String> = vec![];

        for element in parser {
            match element {
                Event::Start(Tag::CodeBlock(CodeBlockKind::Fenced(CowStr::Borrowed(language)))) => {
                    active = language == lang
                }
                Event::End(Tag::CodeBlock(CodeBlockKind::Fenced(CowStr::Borrowed(_)))) => {
                    active = false;
                }
                Event::Text(code) => {
                    if active && !code.contains(&ignore) {
                        result.push(code.to_string());
                    }
                }
                _ => (),
            }
        }

        Ok(result)
    }

    /// Writes a vector of extracted code snippets (each snippet one file: `folder/prefix000.ext`).
    ///
    /// # Arguments
    ///
    /// * `snippets` - The extracted code snippets.
    /// * `folder`   - The target folder for the code files.
    /// * `prefix`   - The prefix of the code files.
    /// * `ext`      - The extension of the code files.
    ///
    pub fn write_snippets(
        self: &Self,
        snippets: Vec<String>,
        folder: String,
        prefix: String,
        ext: String,
    ) -> Result<(), ExitFailure> {
        let mut counter = 0;
        let mut target: String;

        for snippet in snippets {
            target = format!("{}/{}{:0>3}.{}", folder, prefix, counter, ext);
            counter += 1;
            fs::write(Path::new(&target), snippet)
                .with_context(|_| format!("could not write file `{}`", &target))?;
        }

        Ok(())
    }
}
