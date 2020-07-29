// Copyright by Alexander Willner. See the LICENCE file for the license information.

#[cfg(test)]
mod test {

    use md2src::MD2Src;

    #[test]
    fn test_string_to_snippets() {
        let lang = String::from("testlang");
        let ignore = vec![String::from("// ignore me")];
        let markdown = String::from(
r##"
# Markdown Heading

Some code:

```testlang
let i = "Ignore Test Value"; // ignore me
```

```testlang
let s = "Test Value";
```
"##);

        let snippets = MD2Src.get_snippets_from_string(markdown, lang, ignore).unwrap();
        assert!(snippets.iter().any(|code| code.contains(&String::from("Test Value"))), "Should extract snippet");
        assert!(!snippets.iter().any(|code| code.contains(&String::from("Ignore Test Value"))), "Should ignore snippet");
    }

    #[test]
    fn test_file_to_snippets() {
        let lang = String::from("c");
        let ignore = vec![String::from("// ignore me")];
        let file = String::from("tests/test_values.md");

        let snippets = MD2Src.get_snippets_from_file(file, lang, ignore).unwrap();
        assert!(snippets.iter().any(|code| code.contains(&String::from("Hello, C!"))), "Should extract snippet");
        assert!(!snippets.iter().any(|code| code.contains(&String::from("Hello, C! Again!"))), "Should ignore snippet");
    }
}
