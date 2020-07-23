module Yaml.Encode exposing (Encoder, bool, float, int, list, record, string, toString)

import Dict exposing (Dict)
import Yaml.Parser.Ast exposing (Value(..))


type alias EncoderState =
    { col : Int -- Current column
    , indent : Int -- Encoder indent level
    , prefix : Bool -- prefix encoder output
    }


initState : Int -> EncoderState
initState indent =
    { col = 0
    , indent = indent
    , prefix = False
    }


{-| -}
type Encoder a
    = Encoder (EncoderState -> String)


toString : Int -> Encoder a -> String
toString indent =
    internalConvertToString <| initState indent


string : String -> Encoder String
string s =
    Encoder
        (\state ->
            if state.prefix then
                " " ++ s

            else
                s
        )


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
