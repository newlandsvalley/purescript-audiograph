module Test.Audio.Graph.Compiler (compilerSuite) where

import Prelude
import Control.Monad.Free (Free)

import Data.Either (Either(..))
import Audio.Graph.Compiler (compile)

import Test.Unit (Test, TestF, suite, test, success, failure)
import Test.Unit.Assert as Assert

assertCompileError :: forall e. String -> String -> Test e
assertCompileError s expected =
  let
    compileResult =
      compile s
  in
    case compileResult of
      Right res ->
        failure "compiles when it shouldn't"

      Left err ->
        Assert.equal expected err

assertCompiles :: forall e. String -> Test e
assertCompiles s =
  let
    compileResult =
      compile s
  in
    case compileResult of
      Right res ->
        success

      Left err ->
        failure ("compile failed: " <> (show err))

compilerSuite :: forall t. Free (TestF t) Unit
compilerSuite = do
   basicSuite
   badInputSuite

basicSuite :: forall t. Free (TestF t) Unit
basicSuite =
  suite "basic" do
    test "2 nodes" do
      assertCompiles ("Gain id1 { gain 2 } [ output ] " <>
                     "Oscillator id2 {} [ id1 ] End")
    test "3 nodes" do
      assertCompiles ("Gain id1 { gain 2 } [ output ] " <>
                      "BiquadFilter id2 { frequency 400 } [ id1 ] " <>
                      "Oscillator id3 {} [ id2 ]" <>
                      "End")
    test "3 nodes reverse order" do
      assertCompiles ("Oscillator id3 {} [ id2 ]" <>
                      "BiquadFilter id2 { frequency 400 } [ id1 ] " <>
                      "Gain id1 { gain 2 } [ output ] " <>
                      "End")
    test "1 gain audio param" do
      assertCompiles ("Gain id1 { gain [ setValue 2 ] } [ output ] End")
    test "2 gain audio params" do
      assertCompiles ("Gain id1 { gain [ setValueAtTime 2 0.5, setValueAtTime 3 1.2 ] } [ output ] End")
    test "3 gain audio params" do
      assertCompiles ("Gain id1 { gain [ setValue 1, setValueAtTime 3 2, linearRampToValueAtTime 6 4 ] } [ output ] End")
    test "oscillator type" do
      assertCompiles ("Oscillator id1 { type square } [ output ] End")
    test "oscillator frequency" do
      assertCompiles ("Oscillator id1 { frequency 440 } [ output ] End")
    test "biquad filter type" do
      assertCompiles ("BiquadFilter id1 { type lowpass } [ output ] End")
    test "biquad filter frequency" do
      assertCompiles ("BiquadFilter id1 { frequency 80 } [ output ] End")
    test "audio buffer source url" do
      assertCompiles ("AudioBufferSource id1 { url  wav/techno.wav } [ output ] End")
    test "audio buffer source loop" do
      assertCompiles ("AudioBufferSource id1 { loop true } [ output ] End")
    test "delay delayTime" do
      assertCompiles ("Delay id1 { delayTime 2.0 } [ output ] End")


badInputSuite :: forall t. Free (TestF t) Unit
badInputSuite =
  suite "bad input" do
    test "duplicate node id" do
      assertCompileError
        "Gain id1 { gain 3 } [ output ] Oscillator id1 {} [ output ] End"
        "identifier: id1 has already been used"
    test "output is a reserved identifier" do
      assertCompileError
        "Gain output { gain 2 } [ id1 ] Oscillator id1 {} [ output ] End"
        "identifier: output is reserved as the default output node"
    test "unknown node reference" do
      assertCompileError
        "Gain id1 { gain 2 } [ output ] Oscillator id2 {} [ badref ] End"
        "identifier: badref has not been defined"
