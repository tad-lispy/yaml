module Yaml.Encode exposing
    ( Encoder
    , toString
    , string, int, float, bool
    , list, record
    )

{-| Turn Elm values into [YAML](https://yaml.org). The library is structured in a similar way
to `Json.Encode`.


## Table of Contents

  - **Primitives**: [int](#int), [string](#string), [bool](#bool), [float](#float)
  - **Data Structures**: [list](#list), [record](#record)

@docs Encoder


# Run Encoders

@docs toString


# Primitives

@docs string, int, float, bool


# Data Structures

@docs list, record

-}

import Dict exposing (Dict)
import Yaml.Parser.Ast exposing (Value(..))


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
type Encoder a
    = Encoder (EncoderState -> String)



-- RUN ENCODERS


{-| Encode a given Elm value into a YAML formatted string.

The first argument specifies the amount of indentation in the
resulting string.

    toString 0 (int 4) --> "4"

    toString 0 (list int [ 1, 2, 3 ]) --> "[1,2,3]"

-}
toString : Int -> Encoder a -> String
toString indent =
    internalConvertToString <| initState indent



-- PRIMITIVES


{-| Encode a `String` into a YAML string.

    toString 0 (string "") --> ""

    toString 0 (string "hello") --> "hello"

-}
string : String -> Encoder String
string s =
    Encoder
        (\state ->
            if state.prefix then
                " " ++ s

            else
                s
        )


{-| Encode an `Int` into a YAML int.

    toString 0 (int 42) --> "42"

    toString 0 (int -7) --> "-7"

    toString 0 (int 0) --> "0"

-}
int : Int -> Encoder Int
int i =
    Encoder
        (\state ->
            let
                rep =
                    String.fromInt i
            in
            if state.prefix then
                " " ++ rep

            else
                rep
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
float : Float -> Encoder Float
float f =
    Encoder
        (\state ->
            let
                prefix =
                    if state.prefix then
                        " "

                    else
                        ""

                sign =
                    if f < 0 then
                        "-"

                    else
                        ""
            in
            if isNaN f then
                prefix ++ ".nan"

            else if isInfinite f then
                prefix ++ sign ++ ".inf"

            else
                prefix ++ String.fromFloat f
        )


{-| Encode a `Bool` into a YAML bool.

    toString 0 (bool True) --> "true"

    toString 0 (bool False) --> "false"

-}
bool : Bool -> Encoder Bool
bool b =
    Encoder
        (\state ->
            let
                prefix =
                    if state.prefix then
                        " "

                    else
                        ""
            in
            case b of
                True ->
                    prefix ++ "true"

                False ->
                    prefix ++ "false"
        )


{-| Encode a `List` into a YAML list.

    toString 0 (list float [1.1, 2.2, 3.3]) --> "[1.1,2.2,3.3]"

    toString 2 (list string ["a", "b"])
    --> "- a\n- b"

-}
list : (a -> Encoder a) -> List a -> Encoder (List a)
list encode l =
    Encoder
        (\state ->
            case state.indent of
                0 ->
                    encodeInlineList encode l

                _ ->
                    encodeList encode state l
        )


encodeInlineList : (a -> Encoder a) -> List a -> String
encodeInlineList encode l =
    "["
        ++ (List.map (encode >> toString 0) l
                |> String.join ","
           )
        ++ "]"


encodeList : (a -> Encoder a) -> EncoderState -> List a -> String
encodeList encode state l =
    let
        listElement : a -> String
        listElement val =
            "- "
                ++ (internalConvertToString
                        { state | col = state.col + state.indent }
                        << encode
                   )
                    val

        encoded =
            String.join ("\n" ++ String.repeat state.col " ") <| List.map listElement l
    in
    if state.col == 0 then
        encoded

    else
        String.repeat (state.indent - 2) " " ++ encoded


{-| Encode a `Dict` into a YAML record.

    import Dict


    toString 0 (record identity int (Dict.singleton "Sue" 38)) --> "{Sue: 38}"

    toString 2 (record identity string (Dict.fromList [("hello", "foo"), ("world", "bar")]))
    --> "hello: foo\nworld: bar"

-}
record : (k -> String) -> (v -> Encoder v) -> Dict k v -> Encoder (Dict k v)
record key value r =
    Encoder
        (\state ->
            case state.indent of
                0 ->
                    encodeInlineRecord key value r

                _ ->
                    encodeRecord key value state r
        )


encodeInlineRecord : (k -> String) -> (v -> Encoder v) -> Dict k v -> String
encodeInlineRecord key value r =
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


encodeRecord : (k -> String) -> (v -> Encoder v) -> EncoderState -> Dict k v -> String
encodeRecord key value state r =
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


internalConvertToString : EncoderState -> Encoder a -> String
internalConvertToString state (Encoder encoder) =
    encoder state
