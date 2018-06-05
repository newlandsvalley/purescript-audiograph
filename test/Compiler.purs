module Test.Audio.Graph.Compiler (compilerSuite) where

import Prelude
import Control.Monad.Free (Free)

import Data.Either (Either(..))
import Audio.Graph.Compiler (compile)
import Audio.Graph.Parser (PositionedParseError(..))

import Test.Unit (Test, TestF, suite, test, success, failure)
import Test.Unit.Assert as Assert

assertCompileError :: String -> String -> Test
assertCompileError s expected =
  let
    compileResult =
      compile s
  in
    case compileResult of
      Right res ->
        failure "compiles when it shouldn't"

      Left (PositionedParseError ppe) ->
        Assert.equal expected ppe.error

assertCompiles :: String -> Test
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

compilerSuite :: Free TestF Unit
compilerSuite = do
   basicSuite
   badInputSuite

basicSuite :: Free TestF Unit
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
    test "3 nodes with Dynamics Compressor" do
      assertCompiles ( "AudioBufferSource abs { url noise.wav, loop true}  [ compressor ] " <>
                       "DynamicsCompressor compressor { threshold -20 } [ gain ]" <>
                       "Gain gain { gain 2 } [ output ] " <>
                       "End")
    test "1 gain audio param" do
      assertCompiles ("Gain id1 { gain [ setValue 2 ] } [ output ] End")
    test "2 gain audio params" do
      assertCompiles ("Gain id1 { gain [ setValueAtTime 2 0.5, setValueAtTime 3 1.2 ] } [ output ] End")
    test "3 gain audio params" do
      assertCompiles ("Gain id1 { gain [ setValue 1, setValueAtTime 3 2, linearRampToValueAtTime 6 4 ] } [ output ] End")
    test "relative times in audio params" do
      assertCompiles ("Gain id1 { gain [ setValue 1, setValueAtTime 3 t + 2, linearRampToValueAtTime 6 t +4 ] } [ output ] End")
    test "setTargetAtTime" do
      assertCompiles ("Gain id1 { gain [ setTargetAtTime 2 0.5 0.7 ] } [ output ] End")
    test "oscillator type" do
      assertCompiles ("Oscillator id1 { type square } [ output ] End")
    test "oscillator detune" do
      assertCompiles ("Oscillator id1 { detune [ setValue 2 ] } [ output ] End")
    test "oscillator 2 attributes" do
      assertCompiles ("Oscillator id1 { type triangle, frequency 440 } [ output ] End")
    test "biquad filter type" do
      assertCompiles ("BiquadFilter id1 { type lowpass } [ output ] End")
    test "biquad filter frequency" do
      assertCompiles ("BiquadFilter id1 { frequency 80 } [ output ] End")
    test "biquad filter quality" do
      assertCompiles ("BiquadFilter id1 { quality 80 } [ output ] End")
    test "audio buffer source url" do
      assertCompiles ("AudioBufferSource id1 { url  wav/techno.wav } [ output ] End")
    test "audio buffer 2 attributes" do
      assertCompiles ("AudioBufferSource id1 { url  wav/techno.wav, loop true } [ output ] End")
    test "audio buffer 4 attributes" do
      assertCompiles ("AudioBufferSource id1 " <>
          "{ url  wav/techno.wav, loop true, setLoopStart 1.0, setLoopEnd 3.0 } [ output ] End")
    test "delay delayTime" do
      assertCompiles ("Delay id1 { delayTime 2.0 } [ output ] End")
    test "stereo panner pan" do
      assertCompiles ("StereoPanner id1 { pan -0.8 } [ output ] End")
    test "dynamics compressor - complex audio param" do
      assertCompiles ("DynamicsCompressor id1 { threshold [ setValueAtTime 2 -5.0, setValueAtTime 3 1.2 ] } [ output ] End")
    test "dynamics compressor - 5 audio params" do
      assertCompiles ("DynamicsCompressor id1 { threshold -20, knee 30, ratio 10, attack 0.7, release 0.4 } [ output ] End")
    test "dynamics compressor - many and complex" do
      assertCompiles ("DynamicsCompressor id1 { threshold -20," <>
        "knee [setValueAtTime 2 30 ], ratio [ setValueAtTime 2 10 ] } [ output ] End")
    test "convolver" do
      assertCompiles ("Convolver id1 { url  wav/hall.wav, normalize true } [ output ] End")
    test "connect to param" do
      assertCompiles ("Oscillator modulator { frequency 0.8 } [ gain1 ]" <>
                      "Oscillator carrier { frequency 300.0 } [ output ]" <>
                      "Gain gain1 { gain 30.0 } [ carrier.frequency ] " <>
                      "End")
    test "update oscillator" do
      assertCompiles ("Oscillator id2 { frequency 880 } End")


badInputSuite :: Free TestF Unit
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
    test "unknown node in parameter reference" do
      assertCompileError
        "Gain id1 { gain 2 } [ output ] Oscillator id2 {} [ badref.frequency ] End"
        "identifier: badref.frequency has not been defined"
