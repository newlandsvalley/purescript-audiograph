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
      assertParses ("Gain id1 { gain 2 } [ output ] " <>
                    "Oscillator id2 {} [ id1 ] End")
    test "3 nodes" do
      assertParses ("Gain id1 { gain 2 } [ output ] " <>
                    "BiquadFilter id2 { frequency 400 } [ id1 ] " <>
                    "Oscillator id3 {} [ id2 ] End")
    test "1 gain audio param" do
      assertParses ("Gain id1 { gain [ setValue 2 ] } [ output ] End")
    test "2 gain audio params" do
      assertParses ("Gain id1 { gain [ setValueAtTime 2 0.5, setValueAtTime 3 1.2 ] } [ output ] End")
    test "3 gain audio params" do
      assertParses ("Gain id1 { gain [ setValue 1, setValueAtTime 3 2, linearRampToValueAtTime 6 4 ] } [ output ] End")
    test "oscillator type" do
      assertParses ("Oscillator id1 { type square } [ output ] End")
    test "oscillator frequency" do
      assertParses ("Oscillator id1 { frequency 440 } [ output ] End")
    test "biquad filter type" do
      assertParses ("BiquadFilter id1 { type lowpass } [ output ] End")
    test "biquad filter frequency" do
      assertParses ("BiquadFilter id1 { frequency 80 } [ output ] End")


badInputSuite :: forall t. Free (TestF t) Unit
badInputSuite =
  suite "bad input" do
    test "duplicate node id" do
      assertParseError
        "Gain id1 { gain 3 } [ output ] Oscillator id1 {} [ output ] End"
        "identifier: id1 has already been used"
    test "output is a reserved identifier" do
      assertParseError
        "Gain output { gain 2 } [ id1 ] Oscillator id1 {} [ output ] End"
        "identifier: output is reserved as the default output node"
    test "unknown node reference" do
      assertParseError
        "Gain id1 { gain 2 } [ output ] Oscillator id2 {} [ badref ] End"
        "identifier: badref has not been defined"
