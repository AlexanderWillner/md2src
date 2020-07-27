// Copyright by Alexander Willner. See the LICENCE file for the license information.

use exitfailure::ExitFailure;
use failure::ResultExt;
use pulldown_cmark::{CodeBlockKind, CowStr, Event, Options, Parser, Tag};
use std::fs;
use std::path::Path;

pub fn get_snippets(
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

pub fn write_snippets(
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
        fs::write(Path::new(&target), snippet).with_context(|_| format!("could not write file `{}`", &target))?;
    }

    Ok(())
}
