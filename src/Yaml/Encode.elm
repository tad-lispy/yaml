module Yaml.Encode exposing (Encoder, bool, float, int, list, quotedString, string, toString)

import Regex exposing (Regex)
import Yaml.Parser.Ast exposing (Value(..))


type alias EncoderState =
    { col : Int
    , indent : Int
    }


initState : Int -> EncoderState
initState indent =
    { col = 0
    , indent = indent
    }


{-| -}
type Encoder a
    = Encoder (EncoderState -> String)


toString : Int -> Encoder a -> String
toString indent =
    internalConvertToString <| initState indent


string : String -> Encoder String
string s =
    let
        regexFromString : String -> Regex
        regexFromString =
            Regex.fromString >> Maybe.withDefault Regex.never
    in
    case Regex.contains (regexFromString "\\s") s of
        True ->
            quotedString s

        False ->
            Encoder (\_ -> s)


quotedString : String -> Encoder String
quotedString s =
    Encoder (\_ -> "\"" ++ s ++ "\"")


int : Int -> Encoder Int
int i =
    Encoder (\_ -> String.fromInt i)


float : Float -> Encoder Float
float f =
    let
        sign =
            if f < 0 then
                "-"

            else
                "+"
    in
    if isNaN f then
        Encoder (\_ -> ".nan")

    else if isInfinite f then
        Encoder (\_ -> sign ++ ".inf")

    else
        Encoder (\_ -> String.fromFloat f)


bool : Bool -> Encoder Bool
bool b =
    case b of
        True ->
            Encoder (\_ -> "true")

        False ->
            Encoder (\_ -> "false")


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


internalConvertToString : EncoderState -> Encoder a -> String
internalConvertToString state (Encoder encoder) =
    encoder state
