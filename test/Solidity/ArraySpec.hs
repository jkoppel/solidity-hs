module Solidity.ArraySpec (spec) where

import Solidity
import Test.Hspec
import Test.Hspec.Megaparsec
import TestUtils (testParse)

spec :: Spec
spec = parallel $ do
  describe "parses array expressions" $ do
    it "parses slice expression full" $ do
      let input = "foo[64:128]"
      testParse parseExpression input `shouldParse` 
        SliceExpression 
          (IdentifierExpression (Identifier "foo")) 
          (Just (ExpressionLiteral (NumberLiteral 64 Nothing))) 
          (Just (ExpressionLiteral (NumberLiteral 128 Nothing)))

    it "parses slice expression open start" $ do
      let input = "foo[:64]"
      testParse parseExpression input `shouldParse` 
        SliceExpression 
          (IdentifierExpression (Identifier "foo")) 
          Nothing 
          (Just (ExpressionLiteral (NumberLiteral 64 Nothing)))

    it "parses slice expression empty" $ do
      let input = "foo[:]"
      testParse parseExpression input `shouldParse` 
        SliceExpression 
          (IdentifierExpression (Identifier "foo")) 
          Nothing 
          Nothing

    it "parses slice expression open end" $ do
      let input = "foo[64:]"
      testParse parseExpression input `shouldParse` 
        SliceExpression 
          (IdentifierExpression (Identifier "foo")) 
          (Just (ExpressionLiteral (NumberLiteral 64 Nothing))) 
          Nothing
