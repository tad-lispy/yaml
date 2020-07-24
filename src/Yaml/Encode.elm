module Yaml.Encode exposing
    ( Value
    , toString
    , string, int, float, bool
    , list, record, dict
    )

{-| Turn Elm values into [YAML](https://yaml.org). The library is structured in a similar way
to `Json.Encode`.


## Table of Contents

  - **Primitives**: [int](#int), [string](#string), [bool](#bool), [float](#float)
  - **Data Structures**: [list](#list), [record](#record), [dict](#dict)

@docs Value


# Run Encoders

@docs toString


# Primitives

@docs string, int, float, bool


# Data Structures

@docs list, record, dict

-}

import Dict exposing (Dict)


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
    Value
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
int : Int -> Value
int i =
    Value
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
float : Float -> Value
float f =
    Value
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
bool : Bool -> Value
bool b =
    Value
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


    toString 0 (dict identity int (Dict.singleton "Sue" 38)) --> "{Sue: 38}"

    toString 2 (dict identity string (Dict.fromList [("hello", "foo"), ("world", "bar")]))
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
    -- "{name: Sally,height:187}"

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
