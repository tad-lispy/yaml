module Yaml.Encode exposing
    ( Value
    , toString
    , string, int, float, bool, null
    , list, record, dict
    , document
    )

{-| Turn Elm values into [YAML](https://yaml.org). You use `Yaml.Encode` in a very similar
way to how you use `Json.Encode`. For an excellent introduction to encoding (with `Json.Encode`)
have a look at
[this blog post](https://korban.net/posts/elm/2018-09-12-generate-json-from-elm-values-json-encode/).


## Table of Contents

  - **Primitives**: [int](#int), [string](#string), [bool](#bool), [float](#float), [null](#null)
  - **Data Structures**: [list](#list), [record](#record), [dict](#dict)
  - **YAML specifics**: [document](#document)

@docs Value


# Run Encoders

@docs toString


# Primitives

@docs string, int, float, bool, null


# Data Structures

@docs list, record, dict


# YAML specific details

@docs document

-}

import Dict exposing (Dict)
import Yaml.Parser exposing (Value)


{-| Keep track of Encoder state while encoding
-}
type alias EncoderState =
    { col : Int -- Current column
    , indent : Int -- Encoder indent level
    , prefix : Bool -- prefix encoder output
    }


{-| Initialise blank encoder state
-}
initState : Int -> EncoderState
initState indent =
    { col = 0
    , indent = indent
    , prefix = False
    }


{-| A value that knows how to encode Elm values into YAML.
-}
type Value
    = Value (EncoderState -> String)



-- RUN ENCODERS


{-| Encode a given Elm value into a YAML formatted string.

The first argument specifies the amount of indentation in the
resulting string.

    toString 0 (int 4) --> "4"

    toString 0 (list int [ 1, 2, 3 ]) --> "[1,2,3]"

    toString 2 (list int [ 1, 2, 3 ])
    --> "- 1\n- 2\n- 3"

You can also embed your encoded values into a YAML document:

    toString 2 (document
                  <| record [ ( "hello", string "world" ) ])
    --> "---\nhello: world\n..."

-}
toString : Int -> Value -> String
toString indent =
    initState indent
        |> internalConvertToString


internalConvertToString : EncoderState -> Value -> String
internalConvertToString state (Value encoderFn) =
    encoderFn state



-- PRIMITIVES


{-| Encode a `String` into a YAML string.

    toString 0 (string "") --> ""

    toString 0 (string "hello") --> "hello"

-}
string : String -> Value
string s =
    let
        quoted =
            [ "\""
            , String.replace "\"" "\\\"" s
            , "\""
            ]
                |> String.join ""
    in
    Value
        (\state ->
            if String.contains ":" s || String.contains "#" s then
                prefixed " " state quoted

            else
                prefixed " " state s
        )


{-| Encode an `Int` into a YAML int.

    toString 0 (int 42) --> "42"

    toString 0 (int -7) --> "-7"

    toString 0 (int 0) --> "0"

-}
int : Int -> Value
int i =
    Value
        (\state ->
            String.fromInt i
                |> prefixed " " state
        )


{-| Encode a `Float` into a YAML float.

    nan : Float
    nan = (0/0)

    infinity : Float
    infinity = (1/0)

    toString 0 (float 3.14)      --> "3.14"

    toString 0 (float -42)       --> "-42"

    toString 0 (float 0.0)       --> "0"

    toString 0 (float nan)       --> ".nan"

    toString 0 (float -infinity) --> "-.inf"

-}
float : Float -> Value
float f =
    Value
        (\state ->
            let
                sign =
                    if f < 0 then
                        "-"

                    else
                        ""

                val =
                    if isNaN f then
                        ".nan"

                    else if isInfinite f then
                        sign ++ ".inf"

                    else
                        String.fromFloat f
            in
            prefixed " " state val
        )


{-| Encode a `Bool` into a YAML bool.

    toString 0 (bool True) --> "true"

    toString 0 (bool False) --> "false"

-}
bool : Bool -> Value
bool b =
    Value
        (\state ->
            prefixed " "
                state
                (if b then
                    "true"

                 else
                    "false"
                )
        )


{-| Encode a YAML `null` value

    toString 0 null --> "null"

    toString 2 (record [ ("null", null) ])
    --> "null: null"

-}
null : Value
null =
    Value
        (\state ->
            prefixed " " state "null"
        )



-- DATA STRUCTURES


{-| Encode a `List` into a YAML list.

    toString 0 (list float [1.1, 2.2, 3.3])
    --> "[1.1,2.2,3.3]"

    toString 2 (list string ["a", "b"])
    --> "- a\n- b"

-}
list : (a -> Value) -> List a -> Value
list encode l =
    Value
        (\state ->
            case state.indent of
                0 ->
                    encodeInlineList encode l

                _ ->
                    encodeList encode state l
        )


encodeInlineList : (a -> Value) -> List a -> String
encodeInlineList encode l =
    "["
        ++ (List.map (encode >> toString 0) l
                |> String.join ","
           )
        ++ "]"


encodeList : (a -> Value) -> EncoderState -> List a -> String
encodeList encode state l =
    let
        listElement : a -> String
        listElement val =
            "-"
                ++ (internalConvertToString
                        { state | col = state.col + state.indent, prefix = True }
                        << encode
                   )
                    val

        prefix : String
        prefix =
            if state.prefix then
                "\n"

            else
                ""

        indentAfter : String -> String
        indentAfter s =
            s ++ String.repeat state.col " "
    in
    List.map listElement l
        |> String.join (indentAfter "\n")
        |> String.append (indentAfter prefix)


{-| Encode a `Dict` into a YAML record.

    import Dict


    toString 0 (dict
                  identity
                  int (Dict.singleton "Sue" 38))
    --> "{Sue: 38}"

    toString 2 (dict
                  identity
                  string (Dict.fromList [ ("hello", "foo")
                                        , ("world", "bar")
                                        ]
                         )
               )
    --> "hello: foo\nworld: bar"

-}
dict : (k -> String) -> (v -> Value) -> Dict k v -> Value
dict key value r =
    Value
        (\state ->
            case state.indent of
                0 ->
                    encodeInlineDict key value r

                _ ->
                    encodeDict key value state r
        )


encodeInlineDict : (k -> String) -> (v -> Value) -> Dict k v -> String
encodeInlineDict key value r =
    let
        stringify : Dict k v -> List String
        stringify d =
            d
                |> Dict.map (\_ -> value >> toString 0)
                |> Dict.toList
                |> List.map (\( fst, snd ) -> key fst ++ ": " ++ snd)
    in
    "{"
        ++ (stringify r |> String.join ",")
        ++ "}"


encodeDict : (k -> String) -> (v -> Value) -> EncoderState -> Dict k v -> String
encodeDict key value state r =
    let
        recordElement : ( k, v ) -> String
        recordElement ( key_, value_ ) =
            let
                newState =
                    { state | prefix = True, col = state.col + state.indent }
            in
            key key_ ++ ":" ++ (internalConvertToString newState << value) value_

        prefix : String
        prefix =
            if state.prefix then
                "\n"

            else
                ""

        indentAfter : String -> String
        indentAfter s =
            s ++ String.repeat state.col " "
    in
    r
        |> Dict.toList
        |> List.map recordElement
        |> String.join (indentAfter "\n")
        |> String.append (indentAfter prefix)


{-| Encode a YAML record.

    toString 0 (record [ ( "name", string "Sally" )
                       , ( "height", int 187)
                       ]
               )
    --> "{name: Sally,height: 187}"

    toString 2 (record [ ( "foo", int 42 )
                       , ( "bar", float 3.14 )
                       ]
               )
    --> "foo: 42\nbar: 3.14"

-}
record : List ( String, Value ) -> Value
record r =
    Value
        (\state ->
            case state.indent of
                0 ->
                    encodeInlineRecord r

                _ ->
                    encodeRecord state r
        )


encodeInlineRecord : List ( String, Value ) -> String
encodeInlineRecord r =
    let
        stringify : List ( String, Value ) -> List String
        stringify vals =
            List.map
                (\pair ->
                    Tuple.first pair ++ ": " ++ (Tuple.second >> toString 0) pair
                )
                vals
    in
    "{" ++ (stringify r |> String.join ",") ++ "}"


encodeRecord : EncoderState -> List ( String, Value ) -> String
encodeRecord state r =
    let
        recordElement : ( String, Value ) -> String
        recordElement ( key, value ) =
            let
                newState =
                    { state | prefix = True, col = state.col + state.indent }
            in
            key ++ ":" ++ internalConvertToString newState value

        prefix : String
        prefix =
            if state.prefix then
                "\n"

            else
                ""

        indentAfter : String -> String
        indentAfter s =
            s ++ String.repeat state.col " "
    in
    r
        |> List.map recordElement
        |> String.join (indentAfter "\n")
        |> String.append (indentAfter prefix)


{-| Encode a YAML document

YAML "documents" are demarked by "`---`" at the beginning and
"`...`" at the end. This encoder places a value into a
demarcated YAML document.

    toString 0 (document <| string "hello")
    --> "---\nhello\n..."

    toString 2 (document
                  <| record [ ("hello", int 5)
                            , ("foo", int 3)
                            ]
               )
    --> "---\nhello: 5\nfoo: 3\n..."

-}
document : Value -> Value
document val =
    Value
        (\state ->
            "---\n"
                ++ internalConvertToString state val
                ++ "\n..."
        )


prefixed : String -> EncoderState -> String -> String
prefixed prefix state val =
    if state.prefix then
        prefix ++ val

    else
        val
