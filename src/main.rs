// Copyright by Alexander Willner. See the LICENCE file for the license information.

mod cli;

use exitfailure::ExitFailure;
use md2src::MD2Src;
use structopt::StructOpt;

fn cli_entry<T>(args: T) -> Result<(), ExitFailure>
where
    T: Iterator<Item = String>,
{
    let args = cli::Options::from_iter(args);
    let md2src = MD2Src {};
    let snippets = md2src.get_snippets_from_file(args.file, args.lang, args.ignore)?;
    md2src.write_snippets(snippets, args.folder, args.prefix, args.ext)
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
        let args = ["", "tests/test_values.md", "."].iter().map(|s| s.to_string());
        assert!(cli_entry(args).is_ok());
    }
}
