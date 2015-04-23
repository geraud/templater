{-# LANGUAGE OverloadedStrings #-}
module Text.TemplaterSpec (main, spec) where

import Data.Text             (Text)
import Test.Hspec
import Test.Hspec.Attoparsec

import Text.Templater

spec :: Spec
spec = describe "Templater" $ do

   describe "parsing" $ do

      describe "template with all elements" $ do
         it "parses a string" $

            ("Hello %{world}, it is pretty cool" :: Text) ~> templateP `shouldParse`
               [Literal "Hello ", Variable "world", Literal ", it is pretty cool"]

      describe "top level parser" $ do

         it "parses a simple string" $
            ("Hello world" :: Text) ~> templateItemP `shouldParse` Literal "Hello world"

         it "parses a variable" $
            ("%{hello}" :: Text) ~> templateItemP `shouldParse` Variable "hello"

         it "parses an escaped %" $
            ("%%" :: Text) ~> templateItemP `shouldParse` Literal "%"

      describe "literal" $ do

         it "returns a literal of the input" $
            ("Hello world" :: Text) ~> literalItemP `shouldParse` Literal "Hello world"

         it "return a string up to the % char" $
            ("Hello %{world}" :: Text) ~> literalItemP `shouldParse` Literal "Hello "

      describe "variable or escaped % char" $ do

         it "parses a variable" $
            ("%{woot}" :: Text) ~> escapedOrVariableP `shouldParse` Variable "woot"

         it "parses an escaped %" $
            ("%%" :: Text) ~> escapedOrVariableP `shouldParse` Literal "%"

main :: IO ()
main = hspec spec
