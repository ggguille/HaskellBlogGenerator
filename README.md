# Learn Haskell by building a blog generator

We will implement a simple static blog generator in Haskell, converting documents written in our own custom markup language to HTML.

[Learn Haskell by building a blog generator - Book](https://lhbg-book.link/)

[Blog](https://gilmi.me/blog)

## [Building an HTML printer library](https://lhbg-book.link/03-html_printer.html)

In this part we'll explore a few basic building blocks in Haskell, including functions, types and modules, while building a small HTML printer library with which we will later construct HTML pages from our markup blog posts.

```bash
runghc hello.hs > hello.html
```

Resources:

> Util DSL library for HTML - [Lucid](https://hackage.haskell.org/package/lucid)

> Learn more about GHCi - [GHC User Guide](https://downloads.haskell.org/~ghc/9.0.1/docs/html/users_guide/ghci.html)

> Package Repository - [Hackage](https://hackage.haskell.org/)

## [Custom markup language](https://lhbg-book.link/04-markup.html)

In this chapter we will define our own simple markup language and parse documents written in this language into Haskell data structures.

Our markup language will contain the following features:

- Headings: prefix by a number of * characters
- Paragraphs: a group of lines without empty lines in between
- Unordered lists: a group of lines each prefixed with -
- Ordered lists: a group of lines each prefixed with #
- Code blocks: a group of lines each prefixed with >

Resources:

> Find out which module to import - [Hoogle](https://hoogle.haskell.org/)

> [Substitution and Equational Reasoning](https://gilmi.me/blog/post/2020/10/01/substitution-and-equational-reasoning)
