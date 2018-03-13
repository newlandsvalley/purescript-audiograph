module Test.Audio.Graph.Parser (parserSuite) where

import Prelude
import Control.Monad.Free (Free)

import Data.Either (Either(..))
import Audio.Graph.Parser (parse)

import Test.Unit (Test, TestF, suite, test, success, failure)
import Test.Unit.Assert as Assert


assertParseError :: forall e. String -> String -> Test e
assertParseError s expected =
  let
    parseResult =
      parse s
  in
    case parseResult of
      Right res ->
        failure "parses when it shouldn't"

      Left err ->
        Assert.equal expected err

assertParses :: forall e. String -> Test e
assertParses s =
  let
    parseResult =
      parse s
  in
    case parseResult of
      Right res ->
        success

      Left err ->
        failure ("parse failed: " <> (show err))

parserSuite :: forall t. Free (TestF t) Unit
parserSuite = do
   basicSuite
   badInputSuite

basicSuite :: forall t. Free (TestF t) Unit
basicSuite =
  suite "basic" do
    test "2 nodes" do
      assertParses "Oscillator id1 [ output ] Gain id2 [ id1 ] End"

badInputSuite :: forall t. Free (TestF t) Unit
badInputSuite =
  suite "bad input" do
    test "duplicate node id" do
      assertParseError
        "Oscillator id1 [ output ] Gain id1 [ output ] End"
        "identifier: id1 has already been used"
    test "unknown reference" do
      assertParseError
        "Oscillator id1 [ output ] Gain id2 [ badref ] End"
        "identifier: badref has not been defined"
