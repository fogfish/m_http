# Http Monad

A class of Erlang monads which can do http requests with few interesting properties such as composition and laziness. 

[![Build Status](https://secure.travis-ci.org/fogfish/m_http.svg?branch=master)](http://travis-ci.org/fogfish/m_http) 
[![Coverage Status](https://coveralls.io/repos/github/fogfish/m_http/badge.svg?branch=master)](https://coveralls.io/github/fogfish/m_http?branch=master)
[![Hex.pm](https://img.shields.io/hexpm/v/m_http.svg)](https://hex.pm/packages/m_http)

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

The [interface overview](doc/interface.md) provides an introduction to HTTP monad features and use-cases:

* monad abstraction of HTTP request/response pattern
* supports on-demand, lazy evaluation of HTTP I/O
* high-order composition of individual HTTP requests to complex networking computations
* human-friendly, Erlang native and declarative syntax to depict HTTP operations
* implements a declarative approach for testing of RESTful interfaces
* automatically encodes/decodes Erlang Native HTTP payload using `Content-Type` hints
* supports generic transformation to algebraic data types (Erlang records)
* supports non-I/O simulation for unit testing

**Supported MIME types**
 
- [x] application/json
- [x] application/x-ndjson
- [x] application/x-www-form-urlencoded
- [x] text/plain


## Getting started

The latest version of the library is available at its `master` branch. All development, including new features and bug fixes, take place on the `master` branch using forking and pull requests as described in contribution guidelines.

### Installation

If you are using `rebar3` you can include the library in your project with

```erlang
{deps, [m_http]}.
```

or

```erlang
{deps, [
   {m_http, ".*",
      {git, "https://github.com/fogfish/m_http", {branch, master}}
   }
]}.
```

### Usage

Please study the syntax and [HTTP monad interface](doc/interface.md). The library supplies [examples](examples) to demonstrate its abilities. This section is a short references to these features. Note, example project uses [httpbin.org](http://httpbin.org) service.

The following example demonstrates a typical usage scenario. The code uses `_ >` prefix to declare request (what is going to be send to host) and `_ <` prefix to declare expected response. The evaluation of "program" fails if expectations do not match actual response.
 
```erlang
example() ->
   [m_http ||
      %% request specification
      _ > "GET http://example.com",
      _ > "Accept: text/html",

      %% response specification
      _ < 200,
      _ < "Content-Type: text/html; charset=UTF-8",
      _ < '*'
   ].
```

Build and run examples with

```bash
cd examples
make && make run
```

Initiate example application

```erlang
application:ensure_all_started(examples).
```

**Monadic abstraction**

Check [m_http_syntax.erl](examples/src/m_http_syntax.erl), the example guides you thought the basic syntax.

```erlang
%%
%% builds a pure HTTP networking computation without side-effect evaluation
%%
%% #Fun<m_state.1.102573846>
IO = m_http_syntax:example().

%%
%% evaluate a side-effect of the computation
%%
%% {ok, [#{<<"title">> => <<"Wake up to WonderWidgets!">>, ...}]}
m_http:once(IO).
```

**Composition of HTTP monads**

Check [m_http_compose.erl](examples/src/m_http_compose.erl), the examples shows the composition practices of multiple requests.

```erlang
%%
%% builds a pure HTTP networking computation without side-effect evaluation
%%
%% #Fun<m_state.1.102573846>
IO = m_http_compose:hof().

%%
%% evaluate a side-effect of the computation
%%
%% {ok, [#{ua => ..., text => ...}]}
m_http:once(IO).
```

**Encode/Decode**

Check [m_http_auto_codec.erl](examples/src/m_http_auto_codec.erl), the example illustrates libraries ability to decode content to Erlang native representations, including records.

```erlang
%%
%% builds a pure HTTP networking computation without side-effect evaluation
%%
%% #Fun<m_state.1.102573846>
IO = m_http_auto_codec:gen().

%%
%% evaluate a side-effect of the computation
%%
%% {ok,#{<<"hello">> => <<"world">>}}
m_http:once(IO).
```

Previous examples applies automatic codec for generic data structures such as maps, list of pairs. The library also supports semi-auto encode/decode to/from algebraic data structures (records). Please note that current release supports only flats ADTs. 

```erlang
%%
%% builds a pure HTTP networking computation without side-effect evaluation
%%
%% #Fun<m_state.1.102573846>
IO = m_http_auto_codec:adt().

%%
%% evaluate a side-effect of the computation
%%
%% {ok, {adt,<<"world">>}]}
m_http:once(IO).
```

**Declarative testing of RESTfull API**

Check [httpbin_SUITE.erl](examples/test/httpbin_SUITE.erl). The examples shows a libraries ability to develop a test cases for your RESTfull APIs.

**Non I/O simulation**

Check [m_http_mock_SUITE.erl](examples/test/m_http_mock_SUITE.erl). The examples demonstrates how to mock HTTP I/O during unit testing.

### More Information

* Study [category pattern](https://github.com/fogfish/datum/blob/master/doc/category.md) as composition style of development to build a new things from small reusable elements.
* Study syntax and [HTTP monad interface](doc/interface.md)
* Study the [generic representation](https://github.com/fogfish/datum/blob/master/doc/generic.md).


## How To Contribute

The library is [Apache 2.0](LICENSE) licensed and accepts contributions via GitHub pull requests:

1. Fork it
2. Create your feature branch (`git checkout -b my-new-feature`)
3. Commit your changes (`git commit -am 'Added some feature'`)
4. Push to the branch (`git push origin my-new-feature`)
5. Create new Pull Request


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

[![See LICENSE](https://img.shields.io/github/license/fogfish/m_http.svg?style=for-the-badge)](LICENSE)
