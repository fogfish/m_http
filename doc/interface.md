# Http Monad Interface

The HTTP monad is implemented as a [category pattern](https://github.com/fogfish/datum/blob/master/doc/category.md), using `parse_transform` feature. It implements a syntax sugar for monads and "do"-notation in Erlang. You have to explicitly declare usage of macros at your code.

```erlang
-compile({parse_transform, category}).
```

A composition of HTTP primitives within the category are written with the following syntax. Each `Arrow` is a morphism applied to HTTP protocol environment. The implementation of HTTP monad resembles the state category. It defines an abstraction of environments and lenses to focus inside it. In other words, the category represents the environment as an "invisible" side-effect of the composition. 
 
```erlang
[m_http || Arrow1, ..., ArrowN]
```

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

Symbols `_ >` defines writer morphism that focuses inside HTTP protocol environment and defines the request behavior;  `_ <` - reader morphism that focuses on executed side-effect (response) of HTTP protocol (See [Composition with state](https://github.com/fogfish/datum/blob/master/doc/category.md#composition-with-state) and [Composition with transformers](https://github.com/fogfish/datum/blob/master/doc/category.md#composition-with-transformers) for details about syntax). Please note that semantic of symbols is directly adapted from curl and [KATT](https://github.com/for-GET/katt).

