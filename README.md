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

Haskell's ability to create very concise code using abstractions is great once one is familiar with the abstractions. Knowing the monad abstraction, we are now already familiar with the core composition API of many libraries - for example:

- [Concurrent](https://hackage.haskell.org/package/stm) and [asynchronous programming](https://hackage.haskell.org/package/async)
- [Web programming](https://gilmi.me/blog/post/2020/12/05/scotty-bulletin-board)
- [Testing](http://hspec.github.io/)
- [Emulating stateful computation](https://hackage.haskell.org/package/mtl-2.2.2/docs/Control-Monad-State-Lazy.html#g:2)
- [sharing environment between computations](https://hackage.haskell.org/package/mtl-2.2.2/docs/Control-Monad-Reader.html#g:2)
- and many more.

### [Exceptions](https://lhbg-book.link/06-errors_and_files/03-exceptions.html)

The Control.Exception module provides us with the ability to throw exceptions from IO code.

As an aside, Handler uses a concept called [existentially quantified types](https://en.m.wikibooks.org/wiki/Haskell/Existentially_quantified_types) to hide inside it a function that takes an arbitrary type that implements Exception.

### Language extensions

Haskell is a standardized language. However, GHC provides extensions to the language - additional features that aren't covered in the 98 or 2010 standards of Haskell. Features such as syntactic extensions (like LambdaCase above), extensions to the type checker, and more.

These extensions can be added by adding `{-# language <extension-name> #-}` (the language part is case insensitive) to the top of a Haskell source file, or they can be set globally for an entire project by specifying them in the [default-extensions](https://cabal.readthedocs.io/en/3.6/cabal-package.html#pkg-field-default-extensions) section in the `.cabal file`.

The list of language extensions can be found in the [GHC manual](https://ghc.gitlab.haskell.org/ghc/doc/users_guide/exts.html), feel free to browse it, but don't worry about trying to memorize all the extensions.

Resources:

> [Either](https://hackage.haskell.org/package/base-4.12.0.0/docs/Data-Either.html)

> [Traversable](https://hackage.haskell.org/package/base-4.15.0.0/docs/Data-Traversable.html#g:1)

> [Monad](https://hackage.haskell.org/package/base-4.15.0.0/docs/Control-Monad.html#v:-61--60--60-), [monad laws](https://wiki.haskell.org/Monad_laws)

> Monad transformers provide a way to stack monad capabilities on top of one another.
>
> - [ExceptT](https://hackage.haskell.org/package/mtl-2.2.2/docs/Control-Monad-Except.html#g:2)
> - [Except](https://hackage.haskell.org/package/transformers-0.6.0.2/docs/Control-Monad-Trans-Except.html#t:Except), same as Either. (ExceptT e Identity)

> [Exception](https://hackage.haskell.org/package/base-4.15.0.0/docs/Control-Exception.html)

> [IOException](https://hackage.haskell.org/package/base-4.15.0.0/docs/GHC-IO-Exception.html#t:IOException)

> [withFile](https://hackage.haskell.org/package/base-4.17.0.0/docs/System-IO.html#v:withFile)

## [Passing environment variables](https://lhbg-book.link/07-environment.html)

We'd like to add some sort of an environment to keep general information on the blog for various processings, such as the blog name, stylesheet location, and so on.

### Environment

We can represent our environment as a record data type and build it from user input

Resources:

> [ReaderT](https://hackage.haskell.org/package/mtl-2.2.2/docs/Control-Monad-Reader.html#g:2)
 
> [Reader](https://hackage.haskell.org/package/mtl-2.2.2/docs/Control-Monad-Reader.html#g:2)

Sample:

```bash
cabal run HaskellBlogGenerator -- convert-dir --input .\tmp --output .\html-dist --replace 
```

## [Testing](https://lhbg-book.link/08-testing.html)

We want to add some tests to our blog generator. At the very least a few regression tests to make sure that if we extend or change our markup parsing code, HTML generation code, or translation from markup to HTML code, and make a mistake, we'll have a safety net alerting us of issues.

We will use the [hspec](https://hspec.github.io/) testing framework to write our tests. There are other testing frameworks in Haskell, for example [tasty](https://hackage.haskell.org/package/tasty), but I like `hspec`'s documentation, so we'll use that.

[Configuration tests in Cabal](https://cabal.readthedocs.io/en/3.6/cabal-package.html#test-suites)

Resources:

> [Hspec tutorial](https://hspec.github.io/expectations.html)

> [Haddock documentation](https://hackage.haskell.org/package/hspec-expectations-0.8.2/docs/Test-Hspec-Expectations.html)

> [raw-strings-qq](https://hackage.haskell.org/package/raw-strings-qq-1.1/docs/Text-RawString-QQ.html)

> [Hspec manual - Parallel test execution](https://hspec.github.io/parallel-spec-execution.html#running-all-tests-in-parallel-with-hspec-discover)

> [Property testing](https://www.scs.stanford.edu/16wi-cs240h/slides/testing.html)

> [Golden testing](https://ro-che.info/articles/2017-12-04-golden-tests)
