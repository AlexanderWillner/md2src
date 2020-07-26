// Copyright by Alexander Willner. See the LICENCE file for the license information.

mod cli;

use exitfailure::ExitFailure;
use failure::ResultExt;
use pulldown_cmark::{CodeBlockKind, CowStr, Event, Options, Parser, Tag};
use std::fs;
use std::path::Path;
use structopt::StructOpt;

fn md2src<T>(args: T) -> Result<(), ExitFailure>
where
    T: Iterator<Item = String>,
{
    let args = cli::Options::from_iter(args);
    let markdown_input = fs::read_to_string(&args.file)
        .with_context(|_| format!("could not read file `{}`", &args.file))?;
    let parser = Parser::new_ext(&markdown_input, Options::empty());
    let mut active = false;
    let mut counter = 0;
    let mut target: String;

    for element in parser {
        match element {
            Event::Start(Tag::CodeBlock(CodeBlockKind::Fenced(CowStr::Borrowed(lang)))) => {
                active = lang == args.lang
            }
            Event::End(Tag::CodeBlock(CodeBlockKind::Fenced(CowStr::Borrowed(_)))) => {
                active = false;
            }
            Event::Text(code) => {
                if active && !code.contains(&args.ignore) {
                    target = format!(
                        "{}/{}{:0>3}.{}",
                        args.folder, args.prefix, counter, args.ext
                    );
                    counter += 1;
                    fs::write(Path::new(&target), code.to_string())
                        .with_context(|_| format!("could not write file `{}`", &target))?;
                }
            }
            _ => (),
        }
    }

    Ok(())
}

fn main() -> Result<(), ExitFailure> {
    // todo: vec![("rust", "rs"), ("javascript", "js"), ...]
    md2src(std::env::args())
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn test_main() {
        let args = ["", "", ""].iter().map(|s| s.to_string());
        assert!(md2src(args).is_err());
        let args = ["", "examples/hello.md", "."].iter().map(|s| s.to_string());
        assert!(md2src(args).is_ok());
    }
}
