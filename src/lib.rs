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
pub struct MD2Src;

impl MD2Src {
    /// Returns a vector of extracted code snippets.
    ///
    /// # Arguments
    ///
    /// * `markdown` - The markdown string
    /// * `lang`     - The language of the code snippets defined after three backticks
    /// * `ignore`   - Ignore those code snippets that include this string
    ///
    pub fn get_snippets_from_string(
        self: &Self,
        markdown: String,
        lang: String,
        ignore: Vec<String>,
    ) -> Result<Vec<String>, ExitFailure> {
        let parser = Parser::new_ext(&markdown, Options::empty());
        let mut active = false;
        let mut result: Vec<String> = vec![];

        for element in parser {
            if let Event::Start(Tag::CodeBlock(CodeBlockKind::Fenced(CowStr::Borrowed(language)))) =
                element
            {
                active = language == lang
            }
            if let Event::End(Tag::CodeBlock(CodeBlockKind::Fenced(CowStr::Borrowed(_)))) = element
            {
                active = false;
            }
            if let Event::Text(code) = element {
                if active && ignore.iter().all(|s| s.is_empty() || !code.contains(s)) {
                    result.push(code.to_string());
                }
            }
        }

        Ok(result)
    }

    /// Returns a vector of extracted code snippets.
    ///
    /// # Arguments
    ///
    /// * `file`   - The markdown filename
    /// * `lang`   - The language of the code snippets defined after three backticks
    /// * `ignore` - Ignore those code snippets that include this string
    ///
    pub fn get_snippets_from_file(
        self: &Self,
        file: String,
        lang: String,
        ignore: Vec<String>,
    ) -> Result<Vec<String>, ExitFailure> {
        self.get_snippets_from_string(fs::read_to_string(&file)?, lang, ignore)
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
