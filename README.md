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

We can ask GHC to notify us when we accidentally write overlapping patterns, or when we haven't listed enough patterns to match all possible values, by passing the flag **-Wall** to **ghc** or **runghc**.</br>
`runghc hello.hs > hello.html -Wall`

Testing parse markup</br>
`ghci> txt <- readFile "/tmp/sample.txt"`</br>
`print $ parse txt`

Resources:

> Find out which module to import - [Hoogle](https://hoogle.haskell.org/)

> [Substitution and Equational Reasoning](https://gilmi.me/blog/post/2020/10/01/substitution-and-equational-reasoning)

## [Gluing things together](https://lhbg-book.link/05-glue.html)

In this chapter we are going to glue the pieces that we built together and build an actual blog generator. We will:

1. Read markup text from a file
2. Parse the text to a Document
3. Convert the result to our Html EDSL
4. Generate HTML code
5. Write it to file

While doing so, we will learn:

- How to work with IO
- How to import external libraries to process whole directories and create a simple command-line interface

Project description is done in a cabal file. We can ask cabal or stack to generate one for us using `cabal init --libandexe` or `stack new`

The [cabal.project](https://cabal.readthedocs.io/en/3.6/cabal-project.html) and [stack.yaml](https://docs.haskellstack.org/en/stable/yaml_configuration/#project-specific-config) files are used by cabal and stack respectively to add additional information on how to build the package. While cabal.project isn't necessary to use cabal, stack.yaml is necessary in order to use stack, so we will cover it briefly.

Resources:

> More information about imports, see this [wiki article](https://wiki.haskell.org/Import).

> [Monoid](https://hackage.haskell.org/package/base-4.15.0.0/docs/Prelude.html#t:Monoid)

> Haskell's central package archive - [Stackage](https://www.stackage.org/)

> The most popular package managers for Haskell are [cabal](https://cabal.readthedocs.io/en/stable/) and [stack](https://docs.haskellstack.org/en/stable/)

> [Core Haskell Tools](https://gilmi.me/blog/post/2021/08/14/hs-core-tools)

> You can find more licenses if you'd like at [choosealicense.com](choosealicense.com).

> [Cabal Commands](https://cabal.readthedocs.io/en/3.6/cabal-commands.html)

> The `optparse-applicative` package has pretty decent [documentation](https://hackage.haskell.org/package/optparse-applicative-0.16.1.0#optparse-applicative)

> [Functor](https://hackage.haskell.org/package/base-4.15.0.0/docs/Data-Functor.html#t:Functor)

> [Applicative](https://hackage.haskell.org/package/base-4.15.0.0/docs/Control-Applicative.html#t:Applicative)

> You can find the laws for the applicative functors in this article called [Typeclassopedia](https://wiki.haskell.org/Typeclassopedia#Laws_2), which talks about various useful type classes and their laws.

> [Alternative](https://hackage.haskell.org/package/base-4.15.0.0/docs/Control-Applicative.html#t:Alternative)

> [Handle](https://hackage.haskell.org/package/base-4.15.0.0/docs/System-IO.html#t:Handle)

## [Handling errors and multiple files](https://lhbg-book.link/06-errors_and_files.html)

We have left an unimplemented function last chapter, and there are a few more things left for us to do to actually call our program a static blog generator. We still need to process multiple files in a directory and create an index landing page with links to other pages.

Our general strategy for processing whole directories is going to be:

- Create the output directory
- Grab all file names in a directory
- Filter them according to their extension, we want to process txt file and copy other files without modification
- We want to parse each text file, build an index of the result, convert the files to HTML, and write everything to the target directory
- While our parsing function can't really fail, trying to read or write a file to the file-system can fail in several ways. It would be nice if our static blog generator was robust enough that it wouldn't fail completely if one single file gave it some trouble. This is a good opportunity to learn about error handling in Haskell, both in uneffectful code and for I/O code.

In the next few chapters we'll survey the landscape of error handling in Haskell before figuring out the right approach for our use case.
