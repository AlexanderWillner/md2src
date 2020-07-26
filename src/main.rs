// Copyright by Alexander Willner. See the LICENCE file for the license information.

use pulldown_cmark::{CodeBlockKind, CowStr, Event, Options, Parser, Tag};
use std::fs;

fn main() {
    let markdown_input = fs::read_to_string("README.md").expect("File 'README.md' not found.");
    let parser = Parser::new_ext(&markdown_input, Options::empty());
    let mut active: bool = false;
    let mut i = 0;

    for element in parser {
        match element {
            Event::Start(Tag::CodeBlock(CodeBlockKind::Fenced(CowStr::Borrowed("rust")))) => {
                active = true
            }
            Event::End(Tag::CodeBlock(CodeBlockKind::Fenced(CowStr::Borrowed("rust")))) => {
                active = false
            }
            Event::Text(code) => {
                if active && !code.contains("#[doc = \"This will ") {
                    fs::write(format!("code{:0>3}.rs", i), code.to_string())
                        .expect("Can't write file.");
                    i += 1;
                }
            }
            _ => (),
        }
    }
}

#[cfg(test)]
mod test {
    #[test]
    fn test_main() {
        super::main();
    }
}
