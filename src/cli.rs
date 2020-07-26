// Copyright by Alexander Willner. See the LICENCE file for the license information.

use structopt::StructOpt;

#[derive(Debug, StructOpt)]
#[structopt(about, author)]
pub struct Options {
    #[structopt(
        name = "filename",
        help = "Markdown file that contains the code snippets",
        default_value = "README.md"
    )]
    pub file: String,
    #[structopt(name = "folder", help = "Folder for the code snippets", default_value = ".")]
    pub folder: String,
    #[structopt(
        name = "string",
        short = "i",
        long = "ignore",
        help = "Ignore code with this string",
        default_value = "#[doc = \"This will fail",
        required = false
    )]
    pub ignore: String,
    #[structopt(
        name = "prefix",
        short = "p",
        long = "prefix",
        help = "Prefix code files with this string",
        default_value = "code_snippet_",
        required = false
    )]
    pub prefix: String,
    #[structopt(
        name = "extension",
        short = "e",
        long = "extension",
        help = "File extension for code files",
        default_value = "rs",
        required = false
    )]
    pub ext: String,
    #[structopt(
        name = "language",
        short = "l",
        long = "language",
        help = "Code snippet language to extract",
        default_value = "rust",
        required = false
    )]
    pub lang: String,
}
