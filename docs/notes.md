
## implicit tokens

displaying implicit tokens in LSP clients is EXPERIMENTAL.

Specific tokens to be displayed can be controlled in settings:
```
"implicitTokens": [],
```

The possible values to use are:

* `"*"`: display implicit Times character `×`
* `","`: display `Null` around stray commas
* `;;`: display `1` and `All` around `;;`
* `;`: display `Null` after `;`
* `?`: display `□` in place of missing arguments


A good setting would be:
```
"implicitTokens": ["*", ",", ";;", "?"]
```

This will display most implicit tokens, but will NOT display Nulls after ; in CompoundExpressions. Those are annoying.

You may also do:
```
"implicitTokens": ["*", ",", ";;", "?", ";"]
```

to see all implicit tokens.


### implicit token client language

when these characters are sent to clients, they represent a combination of implicit tokens

* `x`: implicit Times
* `z`: implicit Times with space preceding it
* `1`: implicit 1
* `A`: implicit All
* `N`: implicit Null
* `e`: expected operand
* `y`: implicit Times + implicit 1
* `B`: implicit All + implicit Times
* `C`: implicit All + implicit Times + implicit 1
* `f`: expected operand + implicit Times
* `D`: implicit All + implicit 1
