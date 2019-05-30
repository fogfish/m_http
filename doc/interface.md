# Http Monad Interface

The HTTP monad is implemented as a [category pattern](https://github.com/fogfish/datum/blob/master/doc/category.md). `parse_transform` feature implements a syntax sugar for monads and "do"-notation in Erlang. You have to explicitly declare usage of `category` macro at your code.

```erlang
-compile({parse_transform, category}).
```

A composition of HTTP primitives within the category are written with the following syntax.

```erlang
[m_http || Arrow1, ..., ArrowN]
```

Here, each `Arrow` is a morphism applied to HTTP protocol. The implementation of HTTP monad resembles the state monad. It defines an abstraction of environments and lenses to focus inside it. In other words, the category represents the environment as an "invisible" side-effect of the composition. 
 
The example definition of HTTP traffic within monadic form becomes

```erlang
example() ->
   [m_http ||
      _ > ...
      _ > ...

      _ < ...
      _ < ...
   ].
```

Please check [Composition with state](https://github.com/fogfish/datum/blob/master/doc/category.md#composition-with-state) and [Composition with transformers](https://github.com/fogfish/datum/blob/master/doc/category.md#composition-with-transformers) for details about this notation. Please note that semantic of symbols is directly adapted from curl and [KATT](https://github.com/for-GET/katt).

Symbols `>` define writer morphism that focuses inside and reshapes HTTP protocol request. The writer morphism is used to declare HTTP method, destination URL, request headers and payload.

Symbols `<` is reader morphism that focuses into side-effect, HTTP protocol response. The reader morphism is a pattern matcher, is used to match HTTP response code, headers and response payload. It helps us to declare our expectations on the response. The evaluation of "program" fails if expectations do not match actual response.

Please note that `[m_http || Arrow1, ..., ArrowN]` and its compositions returns IO-monad, which implements on-demand lazy I/O. The library implements a helper function `fun m_http:once/1` that evaluates a program. 

## Usage

This section define step-by-step guideline of monad usage.

### Method and URL

Definition of HTTP method and URL are *mandatory*, it has to be a first element in the construction. Use either list or tuple syntax. 

```erlang
[m_http ||
   %%  "Method URL"
   _ > "GET http://example.com",
   ...
]
```

```erlang
[m_http ||
   %%  {atom(), string()}
   _ > {'GET', "http://example.com"},
   ...
]
```

### Request headers

Definition of headers is optional, you can list as many headers as needed using HTTP syntax. 

```erlang
[m_http ||
   ...
   %%  "Header: Value"
   _ > "Accept: application/json",
   ...
]
```

### Request payload

The payload is an optional. The library support any arbitrary binary as payload.

```erlang
[m_http ||
   ...
   %% Any binary
   _ > <<"0123456789abcdef">>,
   ...
]
```

You can also use native Erlang data types (e.g. maps, list of pairs) as egress payload. The library implicitly encodes input structures to binary using `Content-Type` as a hint (See README for full list of supported MIME types)


```erlang
[m_http ||
   ...
   _ > "Content-Type: application/json",
   _ > #{hello => <<"world">>},
   ...
]
```

The library support a serialization of algebraic data types -- Erlang records with help of [generic feature](https://github.com/fogfish/datum/blob/master/doc/generic.md) from datum library.

```erlang
[m_http ||
   ...
   _ > "Content-Type: application/json",
   _ > generic_of:myrecord(#myrecord{ ... })
   ...
]
```

### Response code

Specification of expected HTTP Status Code is *mandatory*, use an integer value to specify expected result. The execution fails if service responds with other value then specified one.

```erlang
[m_http ||
   ...
   %%  integer()
   _ < 200,
   ...
]
```

### Response headers

It is possible to match presence of header in the response or match its entire content. The HTTP monad fails if the matched value do not meet expectations.

```erlang
[m_http ||
   ...
   %% "Header: _"
   _ < "Content-Type: _",
   ...
]
```

```erlang
[m_http ||
   ...
   %% "Header: Value"
   _ < "Content-Type: application/json",
   ...
]
```

### Response payload

The library applies auto decoders for response and returns either binary or native Erlang data structure. The `Content-Type` header give a hint to decoder. The library supports retrieval of either entire response payload with wild-card symbol or parts using [lenses](https://github.com/fogfish/datum/blob/master/doc/lens.md).   

```erlang
[m_http ||
   ...
   %% wild-card symbol, an atom()
   _ < '*'
   ...
]
```

```erlang
[m_http ||
   ...
   %% See datum lenses
   %% https://github.com/fogfish/datum/blob/master/doc/lens.md
   _ < lens:c(lens:at(<<"person">>), lens:at(<<"username">>))
   ...
]
```

Please note two lenses: `fun lens:require/1`, `fun lens:defined/0` they give a framework to in-line validation of the response.

```erlang
[m_http ||
   ...
   %% See datum lenses
   %% https://github.com/fogfish/datum/blob/master/doc/lens.md
   _ < lens:c(lens:at(<<"person">>), lens:at(<<"username">>), lens:defined()),
   _ < lens:c(lens:at(<<"person">>), lens:at(<<"city">>), lens:require(<<"Helsinki">>)),
   ...
]
```

Additionally, you can "lift" response to algebraic data types -- Erlang records. Just defined a record template along with lenses that lifts data-in.

```erlang
[m_http ||
   ...
   _ < #myrecord{
      myfield = lens:c(lens:at(<<"person">>), lens:at(<<"username">>))
   }
]
```

Secondly, you can use generic lens feature to decode the content to ADT

```erlang
[m_http ||
   ...
   _ < labelled:lens(#myrecord{})
]
```


### Example

```erlang
example() ->
   [m_http ||
      _ > "GET http://httpbin.org/json",
      _ > "Accept: application/json",

      _ < 200,
      _ < "Content-Type: application/json",
      _ < lens:c(lens:at(<<"slideshow">>), lens:at(<<"title">>), lens:defined()),
      _ < lens:c(lens:at(<<"slideshow">>), lens:at(<<"slides">>))
   ].
```

## Composition

Lazy composition of multiple HTTP I/O is an essential part of the library. The composition is handled in context of state monad. For example, RESTfull API primitives declared as function, each returns HTTP IO-monad. HOF composition of IO-monads is defined by `m_state`.

```erlang
hof() ->
   [m_state ||
      Token  <- github_access_token(),
      User   <- github_user_profile(Token),
      Orgs   <- github_user_orgs(Token),
      cats:unit(#{user => User, orgs => Orgs})
   ].

github_access_token() ->
   [m_http ||
      ...
   ].

github_user_profile(Token) ->
   [m_http ||
      ...
   ].

github_user_contribution(Token) ->
   [m_http ||
      ...
   ].
```

### Composition with recursion

```erlang
recursive() ->
   [m_state ||
      Head <- fetch(),
      Tail <- recursive(),
      cats:unit(Head ++ Tail)
   ].

fetch() ->
   [m_http ||
      ...
   ].
```

