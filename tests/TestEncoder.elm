module TestEncoder exposing (suite)

import Expect
import Fuzz exposing (int)
import Test
import Yaml.Encode as Encode


suite : Test.Test
suite =
    Test.only <|
        Test.describe "Encoding"
            [ Test.describe "String values"
                [ Test.test "simple string" <|
                    \_ ->
                        Expect.equal "string" (Encode.toString 0 (Encode.string "string"))
                , Test.test "quoted string" <|
                    \_ ->
                        Expect.equal "\"hello world\"" (Encode.toString 0 (Encode.string "hello world"))
                ]
            , Test.describe "Numeric values"
                [ Test.fuzz int "integer" <|
                    \x ->
                        Expect.equal (String.fromInt x) (Encode.toString 0 (Encode.int x))
                ]
            , Test.describe "Lists"
                [ Test.test "inline list of integers" <|
                    \_ ->
                        Expect.equal "[1,2,3]"
                            (Encode.toString 0 (Encode.list Encode.int [ 1, 2, 3 ]))
                , Test.test "list of integers indent 2" <|
                    \_ ->
                        let
                            expected =
                                "- 1\n- 2\n- 3"
                        in
                        Expect.equal expected
                            (Encode.toString 2 (Encode.list Encode.int [ 1, 2, 3 ]))
                , Test.test "list of lists of int" <|
                    \_ ->
                        let
                            expected =
                                "- - 1\n  - 2\n- - 3\n  - 4\n- - 5\n  - 6"

                            encoder =
                                Encode.list (Encode.list Encode.int)
                        in
                        Expect.equal expected
                            (Encode.toString 2
                                (encoder
                                    [ [ 1, 2 ], [ 3, 4 ], [ 5, 6 ] ]
                                )
                            )
                , Test.test "list of lists of int indented 5" <|
                    \_ ->
                        let
                            expected =
                                "-    - 1\n     - 2\n-    - 3\n     - 4\n-    - 5\n     - 6"

                            encoder =
                                Encode.list (Encode.list Encode.int)
                        in
                        Expect.equal expected
                            (Encode.toString 5
                                (encoder
                                    [ [ 1, 2 ], [ 3, 4 ], [ 5, 6 ] ]
                                )
                            )
                , Test.test "list of lists of int indented 3" <|
                    \_ ->
                        let
                            expected =
                                "-  - 1\n   - 2\n-  - 3\n   - 4\n-  - 5\n   - 6"

                            encoder =
                                Encode.list (Encode.list Encode.int)
                        in
                        Expect.equal expected
                            (Encode.toString 3
                                (encoder
                                    [ [ 1, 2 ], [ 3, 4 ], [ 5, 6 ] ]
                                )
                            )
                , Test.test "list of lists of list of int" <|
                    \_ ->
                        let
                            expected =
                                "-  -  - 1\n      - 2\n-  -  - 3\n      - 4"

                            encoder =
                                Encode.list <| Encode.list <| Encode.list Encode.int
                        in
                        Expect.equal expected
                            (Encode.toString 3
                                (encoder
                                    [ [ [ 1, 2 ] ], [ [ 3, 4 ] ] ]
                                )
                            )
                ]
            ]
