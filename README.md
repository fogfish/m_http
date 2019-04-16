# Http Monad

A class of Erlang monads which can do http requests with few interesting properties ... 

## Inspiration

Microservices have become a design style to evolve system architecture in parallel,
implement stable and consistent interfaces. An expressive language is required to 
design the variety of network communication use-cases. A pure functional languages
fits very well to express communication behavior. It gives a rich techniques to hide 
the networking complexity using monads as abstraction. The IO-monads helps us to compose
a chain of network operations and represent them as pure computation, build a new 
things from small reusable elements. This library uses the "do"-notation, so called monadic binding form. It is well know in functional programming languages such as [Haskell](https://en.wikibooks.org/wiki/Haskell/do_notation), [Scala](http://docs.scala-lang.org/tutorials/tour/sequence-comprehensions.html) and [Erlang](https://github.com/fogfish/datum/blob/master/doc/monad.md). The *RESTfull networking* becomes a collection of composed "do"-notation in context of a [state monad](https://acm.wustl.edu/functional/state-monad.php).

The library adapts a human-friendly syntax of HTTP request/response logging/definition used by curl and [KATT](https://github.com/for-GET/katt). 

```
> GET / HTTP/1.1
> Host: example.com
> User-Agent: curl/7.54.0
> Accept: */*
>
< HTTP/1.1 200 OK
< Content-Type: text/html; charset=UTF-8
< Server: ECS (phd/FD58)
< ...
```

This semantic provides an intuitive approach to specify HTTP requests/responses. Adoption of this syntax as Erlang native code is backed by [transformers](https://github.com/fogfish/datum/blob/master/doc/category.md#composition-with-transformers) and provides a rich capability to network programming.


## Key features

tbd.

composition of http
human friendly syntax DSL
auto encoders/decoders
api testing as part of common suite 


## Getting started

The latest version of the library is available at its `master` branch. All development, including new features and bug fixes, take place on the `master` branch using forking and pull requests as described in contribution guidelines.

### Installation

tbd.

### Usage

tbd.

### More Information

* Study [category pattern](https://github.com/fogfish/datum/blob/master/doc/category.md) as composition style of development to build a new things from small reusable elements.


## How To Contribute

The library is `tbd.` licensed and accepts contributions via GitHub pull requests:

* Fork the repository on GitHub
* Read build instructions
* Make a pull request

The build process requires [Erlang/OTP](http://www.erlang.org/downloads) version 19.0 or later and essential build tools.

**Build** and **run** service in your development console. The following command boots Erlang virtual machine and opens Erlang shell.

```bash
git clone https://github.com/fogfish/m_http
cd m_http
make
make run
```

### commit message

The commit message helps us to write a good release note, speed-up review process. The message should address two question what changed and why. The project follows the template defined by chapter [Contributing to a Project](http://git-scm.com/book/ch5-2.html) of Git book.

>
> Short (50 chars or less) summary of changes
>
> More detailed explanatory text, if necessary. Wrap it to about 72 characters or so. In some contexts, the first line is treated as the subject of an email and the rest of the text as the body. The blank line separating the summary from the body is critical (unless you omit the body entirely); tools like rebase can get confused if you run the two together.
> 
> Further paragraphs come after blank lines.
> 
> Bullet points are okay, too
> 
> Typically a hyphen or asterisk is used for the bullet, preceded by a single space, with blank lines in between, but conventions vary here
>
>

### bugs

If you experience any issues with the library, please let us know via [GitHub issues](https://github.com/fogfish/datum/issue). We appreciate detailed and accurate reports that help us to identity and replicate the issue. 

* **Specify** the configuration of your environment. Include which operating system you use and the versions of runtime environments. 

* **Attach** logs, screenshots and exceptions, in possible.

* **Reveal** the steps you took to reproduce the problem, include code snippet or links to your project.


## License

tbd.

