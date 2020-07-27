// Copyright by Alexander Willner. See the LICENCE file for the license information.

mod cli;

use exitfailure::ExitFailure;
use md2src::{get_snippets, write_snippets};
use structopt::StructOpt;

fn cli_entry<T>(args: T) -> Result<(), ExitFailure>
where
    T: Iterator<Item = String>,
{
    let args = cli::Options::from_iter(args);
    let snippets = get_snippets(args.file, args.lang, args.ignore)?;
    write_snippets(snippets, args.folder, args.prefix, args.ext)
}

fn main() -> Result<(), ExitFailure> {
    // todo: vec![("rust", "rs"), ("javascript", "js"), ...]
    cli_entry(std::env::args())
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn test_main() {
        let args = ["", "", ""].iter().map(|s| s.to_string());
        assert!(cli_entry(args).is_err());
        let args = ["", "examples/hello.md", "."].iter().map(|s| s.to_string());
        assert!(cli_entry(args).is_ok());
    }
}
