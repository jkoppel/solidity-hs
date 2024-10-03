module Solidity.ContractSpec (spec) where

import Solidity
import Test.Hspec
import Test.Hspec.Megaparsec
import TestUtils (testParse)

spec :: Spec
spec = parallel $ do
  describe "Parses simple contracts" $ do
    it "parses empty contract" $ do
      testParse parseContract "contract TestName {}"
        `shouldParse` ContractDefinition
          { abstract = False,
            name = Identifier "TestName",
            inheritance = Nothing,
            body = []
          }
    it "parses empty abstract contract" $ do
      testParse parseContract "abstract contract _Abstract123 is Parent {}"
        `shouldParse` ContractDefinition
          { abstract = True,
            name = Identifier "_Abstract123",
            inheritance = Just [InheritanceSpecifier (IdentifierPath [Identifier "Parent"]) Nothing],
            body = []
          }

  describe "Parses using directives" $ do
     it "parses using statement with binders" $ do
      testParse parseContractBody "using {add as +, sub as -} for BalanceDelta global;" `shouldParse`
        [
          CUsing (
            UsingDirective {
              binders=
                UsingDirectiveAliases [
                  UsingDirectiveAlias (IdentifierPath [Identifier "add"]) 
                                      (Just $ ABinaryOp Add),
                  UsingDirectiveAlias (IdentifierPath [Identifier "sub"])
                                      (Just $ ABinaryOp Sub)
                ],
              bound=DirectiveType (IdentifierType $ IdentifierPath [Identifier "BalanceDelta"]),
              global=True
            }
          )
        ]