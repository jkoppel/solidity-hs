module Solidity.YulSpec (spec) where

import Solidity
import Test.Hspec
import Test.Hspec.Megaparsec
import TestUtils (testParse)

spec :: Spec
spec = describe "Yul function definition" $ do
  it "parses a simple Yul function definition" $ do
    let input = "function doAdd(x, y) -> result { result := add(x, y) }"
    let expected = YulFunctionDefinition $ YulFunctionDefinitionDeclaration
          { ident = YulIdentifier (Identifier "doAdd")
          , params = [YulIdentifier (Identifier "x"), YulIdentifier (Identifier "y")]
          , returns = Just [YulIdentifier (Identifier "result")]
          , body = YulBlock
              [ YulAssignment
                  (YulAssignmentDeclaration
                    (YulIdentifierPath [YulIdentifier (Identifier "result")])
                    (YulExpressionFunctionCall
                      (YulFunctionCallDeclaration
                        { ident = YulEvmBuiltin (Identifier "add")
                        , body = [ YulExpressionPath (YulIdentifierPath [YulIdentifier (Identifier "x")])
                                 , YulExpressionPath (YulIdentifierPath [YulIdentifier (Identifier "y")])
                                 ]
                        }
                      )
                    )
                  )
              ]
          }
    testParse parseYulStatement input `shouldParse` expected

  it "parses a Yul function definition without return values" $ do
    let input = "function log(x) { mstore(0, x) }"
    let expected = YulFunctionDefinition $ YulFunctionDefinitionDeclaration
          { ident = YulIdentifier (Identifier "log")
          , params = [YulIdentifier (Identifier "x")]
          , returns = Nothing
          , body = YulBlock
              [ YulFunctionCall
                  (YulFunctionCallDeclaration
                    { ident = YulEvmBuiltin (Identifier "mstore")
                    , body = [ YulExpressionLiteral (YulDecimalNumber 0)
                             , YulExpressionPath (YulIdentifierPath [YulIdentifier (Identifier "x")])
                             ]
                    }
                  )
              ]
          }
    testParse parseYulStatement input `shouldParse` expected


  it "parses a Yul hex literal" $ do
    let input = "hex\"19_01\""
    let expected = YulHexString "19_01"
    testParse parseYulLiteral input `shouldParse` expected

  it "parses a Yul function call with hex literal" $ do
    let input = "mstore(ptr, hex\"19_01\")"
    let expected = YulFunctionCall
          (YulFunctionCallDeclaration
            { ident = YulEvmBuiltin (Identifier "mstore")
            , body = [ YulExpressionPath (YulIdentifierPath [YulIdentifier (Identifier "ptr")])
                     , YulExpressionLiteral (YulHexString "19_01")
                     ]
            }
          )
    testParse parseYulStatement input `shouldParse` expected