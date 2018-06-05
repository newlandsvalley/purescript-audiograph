module Test.Main where

import Prelude
import Effect (Effect)

import Test.Unit (suite)
import Test.Unit.Main (runTest)
import Test.Audio.Graph.Compiler (compilerSuite)

main :: Effect  Unit
main = runTest do
  suite "compiler" do
    compilerSuite
