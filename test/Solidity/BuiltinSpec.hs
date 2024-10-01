module Solidity.BuiltinSpec (spec) where

import Solidity
import Test.Hspec
import Test.Hspec.Megaparsec
import TestUtils (testParse)

spec :: Spec
spec = parallel $ do
  describe "parses builtin functions" $ do
    it "parses abi.decode with array and single value" $ do
        let input = "abi.decode(foo, (uint256[], uint256))"
        testParse parseExpression input `shouldParse`
            FunctionCall
                (MemberAccess
                    (IdentifierExpression (Identifier "abi"))
                    (MemberAccessIdentifier (Identifier "decode")))
                (CommaArguments
                    [ IdentifierExpression (Identifier "foo")
                    , TupleExpression
                        [ Just (TypeExpression (ArrayType (ElementaryType (UnsignedInteger (Just 256))) [ArrayTypeEmpty]))
                        , Just (TypeExpression (ElementaryType (UnsignedInteger (Just 256))))
                        ]
                    ])