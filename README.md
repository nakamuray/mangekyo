% MGK(1)
% NAKAMURA Yoshitaka
% Apr 2017

NAME
====

mangekyo - lenses within pipes

DESCRIPTION
===========

mangekyo is a tool to process JSON and other format of data stream,
through lenses within pipes.

INSTALL
=======

mangekyo is written by haskell.
You can use [stack](https://www.haskellstack.org/) to build and install it.

```console
$ cd mangekyo
$ stack install
```

EXAMPLES
========

hello world:
```console
$ mgk 'yield "hello world"`
"hello world"
```

extract `name` from JSON stream:
```console
$ echo '{"name":"roi"}{"name":"zak"}' | mgk 'map { _^.@name }'
"roi"
"zak"
```

extract from nested object:
```console
$ echo '{"user":{"name":"nakamuray"}}' | mgk 'map { _^.@user.@name }'
"nakamuray"
```

sum `value`:
```console
$ echo '{"value":1}{"value":2}{"value":3}' | mgk 'map { _^.@value } | each { @sum += _ }; yield sum'
6
```

sum `value`, functional way:
```console
$ echo '{"value":1}{"value":2}{"value":3}' | mgk 'map { _^.@value } | fold (+) 0 & yield'
6
```

DATA TYPES
==========

#### string
```
"hello world"
"\u3042\u3044\u3046\u3048\u304a"
```

#### number
```
42
10.1
```

#### bool
```
true
false
```

#### null
```
null
```

#### array
```
[1, 2, 3]
```

#### object
```
{"key": "value"}
```

#### tuple
```
(1, 2, 3)
```

#### function
```
x -> y -> { x + y }
{ _ + 1 }

# function call
f x
f x y
```

#### lens
```
@x
at "x"
```


FUNCTIONS AND OPERATORS
=======================

#### string :: value -> string
Convert `value` to string.

#### number :: value -> number
Convert `value` to number.

#### bool :: value -> bool
Convert `value` to bool.

#### not :: value -> bool
Negate boolean `value`.

#### if :: bool -> then\_function -> else\_function
If `bool` is true value, then call `then_function`, else call `else_function`.

#### when :: bool -> function -> value
If `bool` is true value, then call `function`.

#### unless :: bool -> function -> value
If `bool` is false value, call `function`.

#### pass :: () -> null
Do nothing.

#### exit :: number or () -> ()
Exit with return code.

#### id :: value -> value
Return `value` as is.

#### const :: value -> function
Return function which return `value`

#### for :: array -> function -> array
Call `function` for each `array` element, return array of return values.

#### items :: object -> array
Return array of (key, value) tuple.

#### fmt :: string -> string
Format string with namespace variables.

```console
$ mgk '@name .= "world"; yield $ fmt "hello #{name}"'
"hello world"
```

#### split :: sep\_string -> string -> array
Split `string` by `sep_string`.

#### negative :: number -> number
Negate a `number`.

#### length :: value -> number
Return length of `value`.

#### system :: string\_or\_array -> number
Execute system command and return exit status.

#### p :: value -> ()
Print `value` directly (for debug purpose)

#### . :: function -> function -> function
Compose functions (or lenses).

#### $ :: function -> value -> value
Apply `function` to the `value`.

#### & :: value -> function -> value
Apply `function` to the `value`.

#### + :: value -> value -> value
Add values.

#### * :: value -> number -> value
TODO.

#### / :: number -> number -> number
TODO.

#### == :: value -> value -> bool
Left `value` equal right `value`.

#### > :: value -> value -> bool
Left `value` greater than right `value`.

#### \< :: value -> value -> bool
Left `value` less than right `value`.

#### >= :: value -> value -> bool: greater than equal
Left `value` greater than equal right `value`.

#### \<= :: value -> value -> bool
Left `value` less than equal right `value`.

#### =~ :: string -> regex\_string -> array
When `string` match `regex_string`, return array of matched string and captures.
  if not, return null.

#### !~ :: string -> regex\_string -> bool
Not match the string. this function return bool.

PIPE FUNCTIONS
==============

In addition to return value, functions could have side effects:
consume values from upstream, and produce value to downstream.

By default, upstream is `stdin` and downstream is `stdout`.
You can use `|` to join functions, in such case left one is upstream and right
one is downstream.

#### yield :: value -> ()
Send a `value` downstream.
```console
$ mgk 'yield 42'
42
```

#### await :: () -> value
Wait for a value from upstream.
```console
$ seq 10 | mgk 'yield $ await()'
1
```

#### fold :: function -> value -> value
Fold input stream to value.
```console
$ seq 3 | mgk '@r .= fold (+) 0; yield r'
6
```

#### map :: function -> ()
Apply `function` to all values in a stream.
```console
$ seq 3 | mgk 'map { _ * 2 }'
2
4
6
```

#### filter :: function -> ()
Filter stream by predicate `function`.
```console
$ seq 5 | mgk 'filter { _ < 3 }'
1
2
```

#### exclude :: function -> ()
Exclude stream by predicate `function`.
```console
$ seq 5 | mgk 'exclude { _ < 3 }'
3
4
5
```

#### each :: function -> ()
Apply `function` for each value in a stream.
Don't pass result values to downstream, by itself.
```console
$ seq 3 | mgk 'each { _ * 2 }'
$ seq 3 | mgk 'each { yield $ _ * 2 }'
2
4
6
```

#### concat :: () -> ()
Convert stream of array of value, to stream of value.
```console
$ mgk 'yield [1, 2, 3]'
[   
    1,
    2,
    3
]
$ mgk 'yield [1, 2, 3] | concat()'
1
2
3
```

#### consume :: () -> array
Consume all values from stream and return as a array.
```console
$ seq 3 | mgk '@a .= consume(); yield a'
[
    1,
    2,
    3
]
```

#### concatMap :: function -> ()
Map a `function` and concat a result.
```console
$ seq 3 | mgk 'concatMap { [_, _] }'
1
1
2
2
3
3
```

#### isolate :: number -> ()
Isolate given number of items to downstream.
```console
$ seq 10 | mgk 'isolate 3'
1
2
3
```

#### chunksOf :: number -> ()
Group a stream into chunks of given size.
```console
$ seq 4 | mgk 'chunksOf 2'
[
    1,
    2
]
[
    3,
    4
]
```

#### iterate :: function -> value -> ()
Produce an infinite stream of repeated application of function to value.
```console
$ mgk 'iterate { _ + 1 } 0'
1
2
3
4
...
```

#### mergeSource :: function -> ()
Call a function and merge it as a stream to upstream.
```console
$ seq 3 | mgk 'mergeSource { yield "a"; yield "b" }'
[
    "a",
    1
]
[
    "b",
    2
]
```

#### merge :: function -> ()
Alias for `mergeSource`.

#### zipConduit :: function -> function -> (value, value)
Provide every values from upstream to both functions.
Return result of both functions.
```console
$ seq 5 | mgk '@r .= zipConduit { fold (+) 0 } { fold (*) 1 }; yield r'
[   
    15,
    120
]
```

#### zip :: function -> function -> (value, value)
Alias for `zipConduit`.

#### sourceArray :: array -> ()
Provide each element of array to downstream.
```console
$ mgk 'sourceArray [1, 2, 3]'
1
2
3
```

#### source :: array -> ()
Alias for `sourceArray`.

#### leftover :: value -> ()
Get back a value to upstream, which to be consumed by next component.
```console
$ seq 3 | mgk 'leftover 10; map { _ * 2 }'
20
2
4
6
```

#### replicate :: number -> value -> ()
Replicate a value given number of times, provide those to downstream.
```console
$ mgk 'replicate 3 "hello"'
"hello"
"hello"
"hello"
```

#### peek :: () -> value
Look at the next value in the upstream.

LENS FUNCTIONS AND OPERATORS
============================

mangekyo uses lens to get/set value from/to object and array.
It also use lens to modify namespace object.

#### at :: value -> lens
Create a `lens` of given `value`.

#### view :: object or array -> lens -> value
View `object` or `array` using `lens`.
```console
$ mgk 'yield $ view {"key": "value"} @key'
"value"
$ mgk 'yield $ view [1, 2, 3] @1'
2
```

#### set :: lens or setter -> value -> object or array -> value
Set `value` to `object` or `array` using `lens`.
```console
$ mgk 'yield $ set @key "value" {}'
{
    "key": "value"
}
```

#### over :: lens or setter -> function -> object or array -> value
Call `function` over the value lens views, replace it with result value.
```console
$ mgk 'yield $ over @key { _ + 1 } {"key": 1}'
{
    "key": 2
}
```

#### mapped :: setter
Setter for each element of array.
```console
$ mgk 'yield $ set mapped 42 [1, 2, 3]'
[
    42,
    42,
    42
]
$ mgk 'yield $ over mapped { _ + 1 } [1, 2, 3]'  
[
    2,
    3,
    4
]
```

#### ^. :: object or array -> value -> value
Opereter version of `view`.
```console
$ mgk 'yield $ {"key": "value"} ^. @key'
"value"
$ mgk 'yield $ [1, 2, 3] ^. @1'
2
```

#### .~ :: lens or setter -> value -> object or array -> value
Operator version of `set`.
```console
$ mgk 'yield $ (@key .~ "value") {}'
{
    "key": "value"
}
$ mgk 'yield $ {} & @key .~ "value"'
{
    "key": "value"
}
```

#### %~ :: lens or setter -> function -> value -> value
Operator version of over.
```console
$ mgk 'yield $ (@key %~ { _ + 1 }) {"key": 1}'
{
    "key": 2
}
$ mgk 'yield $ {"key": 1} & @key %~ { _ + 1 }'
{
    "key": 2
}
```

#### .= :: lens or setter -> value -> value
`set` to the target of lens within namespace object.
```console
$ mgk '@x .= 10; yield x'
10
```

#### %= :: lens or setter -> function -> value
`over` to the target of lens within namespace object.
```console
$ mgk '@x .= 10; @x %= { _ + 1 }; yield x'
11
```

#### += :: lens or setter -> value -> value
Add `value` to the target of lens within namespace object.
