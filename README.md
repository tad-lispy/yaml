# YAML in Elm

Convert between type-safe Elm values and [YAML](https://yaml.org).

This is forked from [terezka/yaml](https://package.elm-lang.org/packages/terezka/yaml/latest/).

## Install

```bash
$ elm install MaybeJustJames/yaml
```

and import the library in an elm file like this

```elm
import Yaml.Decode -- for decoders
```

## Documentation

Find the documentation on [Elm's package website](http://package.elm-lang.org/packages/MaybeJustJames/yaml/latest).

## Example Usage

Say you have some YAML which looks like this:

```yaml
---
- name:
    first: Marie
    last: Curie
  occupation: [ chemist, physicist ]
  age: 66
  children: [ Irène, Ève ]
- name:
    first: Alva
    last: Myrdal
  occupation: [ sociologist, diplomat, politician ]
  age: 84
  children: []
- name:
    first: Svetlana
    last: Alexievich
  occupation: [ journalist, historian ]
  age: 72
  children: []
...  
```

to decode this, you could write

```elm
import Yaml.Decode (..)

type alias Woman =
  { name : String
  , occupation : List String
  , age : Int
  , children : Int -- number of children
  }

decoder : Decoder Woman
decoder =
  map4 Woman
    (map2 (\first last -> first ++ " " ++ last)
          (at ["name", "first"] string)
          (at ["name", "last"] string))
    (field "occupation" (list string))
    (field "age" int)
    (map List.length (field "children" (list string)))


fromString
  (list decoder)
  yamlString -- The string containing the YAML example above

```

## Development

The branch `parser-logging` contains a version of the
[parser logger](https://discourse.elm-lang.org/t/improved-parser-logger/5964)
by @Janiczek.

This, along with writing detailed tests using [elm-test](https://github.com/elm-community/elm-test)
is how I've been developing this package.

Please feel encouraged and welcome to submit bugs, PRs, etc.


## Major Missing Features

- `Yaml.Encode` to encode Elm values into YAML. [#12](https://github.com/MaybeJustJames/yaml/issues/12)
- Testing against the official [YAML test suite](https://github.com/yaml/yaml-test-suite). [#7](https://github.com/MaybeJustJames/yaml/issues/7)

## Copying

You are free to copy, modify, and distribute this package with attribution under the terms of the MIT license. See the [LICENSE](LICENSE) file for details.
