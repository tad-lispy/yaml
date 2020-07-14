module TestDecoder exposing (suite)

import Dict
import Expect
import Fuzz exposing (bool, float, int, list, string)
import Test
import Yaml.Decode as Yaml


suite : Test.Test
suite =
    Test.describe "Decoding"
        [ Test.describe "String values"
            [ Test.test "unquoted string" <|
                \_ -> given "string" Yaml.string |> expectEqual "string"
            , Test.test "single-quoted string" <|
                \_ -> given "'string'" Yaml.string |> expectEqual "string"
            , Test.test "double-quoted string" <|
                \_ -> given "\"string\"" Yaml.string |> expectEqual "string"
            , Test.test "quoted number" <|
                \_ -> given "'5'" Yaml.string |> expectEqual "5"
            , Test.test "unquoted number" <|
                \_ -> given "0" Yaml.string |> expectFail "Expected string"
            , Test.test "unquoted bool" <|
                \_ -> given "true" Yaml.string |> expectFail "Expected string"
            ]
        , Test.describe "boolean values"
            [ Test.test "boolean true" <|
                \_ -> given "true" Yaml.bool |> expectEqual True
            , Test.test "boolean True" <|
                \_ -> given "True" Yaml.bool |> expectEqual True
            , Test.test "boolean TRUE" <|
                \_ -> given "TRUE" Yaml.bool |> expectEqual True
            , Test.test "boolean false" <|
                \_ -> given "false" Yaml.bool |> expectEqual False
            , Test.test "boolean False" <|
                \_ -> given "False" Yaml.bool |> expectEqual False
            , Test.test "boolean FALSE" <|
                \_ -> given "FALSE" Yaml.bool |> expectEqual False
            , Test.test "empty" <|
                \_ -> given "" Yaml.bool |> expectFail "Expected bool"
            , Test.test "non-boolean string" <|
                \_ -> given "rubbish" Yaml.bool |> expectFail "Expected bool"
            , Test.test "non-boolean number" <|
                \_ -> given "3" Yaml.bool |> expectFail "Expected bool"
            ]
        , Test.describe "numeric values"
            [ Test.fuzz int "integers" <|
                \x -> given (String.fromInt x) Yaml.int |> expectEqual x
            , Test.test "float as integer" <|
                \_ -> given "2.1" Yaml.int |> expectFail "Expected int"
            , Test.test "rubbish as integer" <|
                \_ -> given "rubbish" Yaml.int |> expectFail "Expected int"
            , Test.test "empty string as integer" <|
                \_ -> given "" Yaml.int |> expectFail "Expected int"
            , Test.fuzz float "floats" <|
                \x ->
                    let
                        strFloat =
                            if x == 0.0 then
                                "0.0"

                            else
                                String.fromFloat x
                    in
                    given strFloat Yaml.float |> expectEqual x
            , Test.test "integer as float" <|
                \_ -> given "0" Yaml.float |> expectFail "Expected float"
            , Test.test "rubbish as float" <|
                \_ -> given "rubbish" Yaml.float |> expectFail "Expected float"
            , Test.test "Empty string as float" <|
                \_ -> given "" Yaml.float |> expectFail "Expected float"
            ]
        , Test.describe "null and nullable"
            [ Test.test "empty string as null" <|
                \_ -> given "" Yaml.null |> expectEqual Maybe.Nothing
            , Test.test "whitespace as null" <|
                \_ -> given "  " Yaml.null |> expectEqual Maybe.Nothing
            , Test.test "null as null" <|
                \_ -> given " null " Yaml.null |> expectEqual Maybe.Nothing
            , Test.test "non-empty string as null" <|
                \_ -> given "str" Yaml.null |> expectFail "Expected null"
            , Test.test "nullable string" <|
                \_ -> given "" (Yaml.nullable Yaml.string) |> expectEqual Maybe.Nothing
            , Test.test "nullable bool" <|
                \_ -> given "" (Yaml.nullable Yaml.bool) |> expectEqual Maybe.Nothing
            , Test.test "nullable int" <|
                \_ -> given "" (Yaml.nullable Yaml.int) |> expectEqual Maybe.Nothing
            , Test.test "nullable float" <|
                \_ -> given "" (Yaml.nullable Yaml.float) |> expectEqual Maybe.Nothing
            ]
        , Test.describe "fields and nesting"
            [ Test.test "access first existing field" <|
                \_ -> given "hello: 5\nworld:6" (Yaml.field "hello" Yaml.int) |> expectEqual 5
            , Test.test "access second existing field" <|
                \_ -> given "hello: 5\nworld:6" (Yaml.field "world" Yaml.int) |> expectEqual 6
            , Test.test "access a field that does not exist" <|
                \_ -> given "hello: 5\nworld:6" (Yaml.field "fake" Yaml.int) |> expectFail "Expected property: fake"
            , Test.test "access an existing nested field" <|
                \_ -> given "hello:\n  world: 2" (Yaml.at [ "hello", "world" ] Yaml.int) |> expectEqual 2
            , Test.test "access a nested field that does not exist" <|
                \_ ->
                    given "hello:\n  world: 2"
                        (Yaml.at [ "hello", "world", "foo" ] Yaml.int)
                        |> expectFail "Expected record"
            ]
        , Test.describe "lists"
            [ Test.test "empty list" <|
                \_ -> given "[]" (Yaml.list Yaml.null) |> expectEqual []
            , Test.fuzz (list int) "list of integers" <|
                \xs ->
                    let
                        strList : String
                        strList =
                            "[" ++ String.join "," (List.map String.fromInt xs) ++ "]"
                    in
                    given
                        strList
                        (Yaml.list Yaml.int)
                        |> expectEqual xs
            , Test.fuzz (list string) "list of strings" <|
                \xs ->
                    let
                        strList : String
                        strList =
                            "[" ++ String.join ", " xs ++ "]"
                    in
                    given strList (Yaml.list Yaml.string) |> expectEqual xs
            , Test.fuzz (list bool) "list of boolean values" <|
                \xs ->
                    let
                        strList : String
                        strList =
                            "["
                                ++ String.join ", "
                                    (List.map
                                        (\b ->
                                            if b then
                                                "true"

                                            else
                                                "false"
                                        )
                                        xs
                                    )
                                ++ "]"
                    in
                    given strList (Yaml.list Yaml.bool) |> expectEqual xs
            ]
        ]


{-| Utility function that sets up a test.
-}
given : String -> Yaml.Decoder a -> Result Yaml.Error a
given input decoder =
    Yaml.fromString decoder input


{-| Utility function that checks for equality between the result
of a test and the expected value.
-}
expectEqual : a -> Result Yaml.Error a -> Expect.Expectation
expectEqual expected got =
    Expect.equal (Ok expected) got


{-| Utility function that checks the failure mode of a Decoder
-}
expectFail : String -> Result Yaml.Error a -> Expect.Expectation
expectFail expected got =
    Expect.equal (Err (Yaml.Decoding expected)) got
