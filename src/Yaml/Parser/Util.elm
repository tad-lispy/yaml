module Yaml.Parser.Util exposing 
  ( isColon, isComma, isDot, isDash, isHash, isSpace, isNewLine, isListStart, isListEnd, isRecordStart, isRecordEnd, either, neither, neither3
  , threeDashes, threeDots, spaces, whitespace, multiline
  , singleQuotes, doubleQuotes, remaining
  , indented, postProcessString
  )

import Parser as P exposing ((|=), (|.))
import Regex exposing (Regex)

-- LOGGING
log : String -> P.Parser a -> P.Parser a
log message loggedParser =
        P.succeed
            (\source offsetBefore ->
                let
                    _ =
                        Debug.log "+++++++++++++++++ starting" message
                in
                ( source, offsetBefore )
            )
            |= P.getSource
            |= P.getOffset
            |> P.andThen
                (\( source, offsetBefore ) ->
                    {- Kinda like that logging decoder from Thoughtbot:
                       https://thoughtbot.com/blog/debugging-dom-event-handlers-in-elm

                       Basically we run `Parser.run` ourselves so that we can
                       get at the context and say something more meaningful
                       about the failure if it happens.
                    -}
                    let
                        remainingSource =
                            String.dropLeft offsetBefore source
                                |> Debug.log "yet to parse  "
                    in
                    let
                        parseResult =
                            -- the side-effecty part; this might log lies
                            P.run
                                (P.succeed
                                    (\parseResult_ innerOffset ->
                                        let
                                            _ =
                                                Debug.log "chomped string" (String.left innerOffset remainingSource)
                                        in
                                        parseResult_
                                    )
                                    |= loggedParser
                                    |= P.getOffset
                                )
                                remainingSource
                                |> Debug.log "parse result  "
                    in
                    let
                        _ =
                            Debug.log "----------------- ending  " message
                    in
                    loggedParser
                )


-- QUESTIONS


{-| -}
isColon : Char -> Bool
isColon =
  is ':'


{-| -}
isComma : Char -> Bool
isComma =
  is ','


{-| -}
isDot : Char -> Bool
isDot =
  is '.'


{-| -}
isDash : Char -> Bool
isDash =
  is '-'


{-| -}
isHash : Char -> Bool
isHash =
  is '#'


{-| -}
isSpace : Char -> Bool
isSpace =
  is ' '


{-| -}
isNewLine : Char -> Bool
isNewLine =
  is '\n'


{-| -}
isListStart : Char -> Bool
isListStart =
  is '['


{-| -}
isListEnd : Char -> Bool
isListEnd =
  is ']'


{-| -}
isRecordStart : Char -> Bool
isRecordStart =
  is '{'


{-| -}
isRecordEnd : Char -> Bool
isRecordEnd =
  is '}'


{-| -}
isSingleQuote : Char -> Bool
isSingleQuote =
  is '\''


{-| -}
isDoubleQuote : Char -> Bool
isDoubleQuote =
  is '"'


{-| -}
either : (Char -> Bool) -> (Char -> Bool) -> Char -> Bool
either f1 f2 char =
  f1 char || f2 char


{-| -}
neither : (Char -> Bool) -> (Char -> Bool) -> Char -> Bool
neither f1 f2 char =
  not (f1 char) && not (f2 char)


{-| -}
neither3 : (Char -> Bool) -> (Char -> Bool) -> (Char -> Bool) -> Char -> Bool
neither3 f1 f2 f3 char =
  not (f1 char) && not (f2 char) && not (f3 char)


{-| -}
is : Char -> Char -> Bool
is searched char =
  char == searched



-- 


{-| -}
threeDashes : P.Parser ()
threeDashes =
  P.symbol "---"


{-| -}
threeDots : P.Parser ()
threeDots =
  P.symbol "..."


{-| -}
spaces : P.Parser ()
spaces =
  P.chompWhile isSpace


{-| -}
whitespace : P.Parser ()
whitespace =
  let
    step _ =
      P.oneOf
        [ P.succeed (P.Loop ()) 
            |. comment
        , P.succeed (P.Loop ()) 
            |. P.chompIf isSpace
        , P.succeed (P.Loop ()) 
            |. P.chompIf isNewLine
        , P.succeed (P.Done ())
        ]
  in
  P.loop () step
  

{-| -}
comment : P.Parser ()
comment =
  P.lineComment "#"



-- STRINGS


{-| -}
multiline : Int -> P.Parser String
multiline indent =
  P.loop [] (multilineStep indent)


multilineStep : Int -> List String -> P.Parser (P.Step (List String) String)
multilineStep indent lines =
  let
      multilineString : List String -> String
      multilineString lines_ =
          String.join " " (List.reverse lines_)

      conclusion line indent_ =
          if indent_ > indent then
              P.Loop (line :: lines)
          else
              P.Done (multilineString (line :: lines))
  in
      P.oneOf
          [ P.succeed conclusion
            |= characters (not << isNewLine)
            |. P.chompIf isNewLine
            |. spaces
            |= P.getCol
          , P.succeed (P.Done <| multilineString lines)
          ]


{-| -}
characters : (Char -> Bool) -> P.Parser String
characters isOk =
  let
    done chars =
      chars
        |> List.reverse
        |> String.concat
        |> P.Done

    more chars char =
      char :: chars
        |> P.Loop

    step chars =
      P.oneOf
        [ P.succeed (done chars)
            |. comment
        , P.succeed ()
            |. P.chompIf isOk
            |> P.getChompedString
            |> P.map (more chars)
        , P.succeed (done chars)
        ]
  in
  P.loop [] step


{-| -}
characters_ : (Char -> Bool) -> P.Parser String
characters_ isOk =
  P.succeed ()
    |. P.chompWhile isOk
    |> P.getChompedString


{-| -}
singleQuotes : P.Parser String
singleQuotes =
  P.succeed (String.replace "\\" "\\\\")
    |. P.symbol "'"
    |= characters_ (not << isSingleQuote)
    |. P.symbol "'"
    |. spaces


{-| -}
doubleQuotes : P.Parser String
doubleQuotes =
  P.succeed identity
    |. P.symbol "\""
    |= characters_ (not << isDoubleQuote)
    |. P.symbol "\""
    |. spaces


{-| -}
remaining : P.Parser String
remaining =
  P.succeed ()
    |. P.chompUntilEndOr "\n...\n"
    |> P.getChompedString

postProcessString : String -> String
postProcessString str =
    let
        regexFromString : String -> Regex
        regexFromString =
            Regex.fromString >> Maybe.withDefault Regex.never
    in
    str
        |> Regex.replace (regexFromString "\\s\\s+")
           (\match ->
                if String.contains "\n\n" match.match then
                    "\n"
                else
                    " "
                )


-- INDENT


{-| -}
indented : Int -> { smaller : P.Parser a, exactly : P.Parser a, larger : Int -> P.Parser a, ending : P.Parser a } -> P.Parser a
indented indent next =
  let check actual =
        P.oneOf
          [ P.andThen (\_ -> next.ending) P.end
          , P.andThen (\_ -> next.ending) (P.symbol "\n...\n")
          , if actual == indent then next.exactly
            else if actual > indent then next.larger actual
            else next.smaller
          ]
  in
  P.succeed identity
    |. whitespace
    |= P.getCol
    |> P.andThen check
