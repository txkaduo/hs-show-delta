module Data.ShowDelta.THSpec (main, spec) where

import Data.Ratio ((%))
import Data.String
import Data.Text ()
import Test.Hspec
import Test.QuickCheck
import Text.Printf

import Data.ShowDelta.Class
import Data.ShowDelta.TH

data X = X
$(deriveShowDelta ''X)

data X1 = X1 Int
$(deriveShowDelta ''X1)

data XY = MkX Int
        | MkY String
        deriving (Eq, Show)

$(deriveShowDelta ''XY)

data Record = Record
  { intField :: Int
  , stringField :: String
  }
$(deriveShowDelta ''Record)

newtype N = N Int
$(deriveShowDelta ''N)

newtype NR = NR { unNR :: Int}
$(deriveShowDelta ''NR)

spec :: Spec
spec = do
  describe "Composite Types" $ do
    it "can show delta of list" $ do
      let d = showDelta [1 :: Int] [1, 2]
      d `shouldBe` Just "[ 1: _ => 2 ]"

    it "can show delta of tuples" $ do
      let d = showDelta (1 :: Int, "x" :: String) (2, "y")
      d `shouldBe` Just "(1 => 2, \"x\" => \"y\")"

  describe "X" $ do
    it "can show simple delta" $ do
      let d = showDelta X X
      d `shouldBe` Nothing

  describe "X1" $ do
    it "can show simple delta" $ do
      let d = showDelta (X1 1) (X1 2)
      d `shouldBe` (Just "X1 { 1 => 2 }")

  describe "Newtype" $ do
    it "can show simple delta" $ do
      let d = showDelta (N 1) (N 2)
      d `shouldBe` (Just "N { 1 => 2 }")

    it "can show delta of fields" $ do
      let d = showDelta (NR 1) (NR 2)
      d `shouldBe` (Just "NR { unNR: 1 => 2 }")

  describe "XY" $ do
    it "can should delta with same ctors" $ do
      let d = showDelta (MkX 1) (MkX 2)
      d `shouldBe` (Just "MkX { 1 => 2 }")

    it "can should delta with different ctors" $ do
      let d = showDelta (MkX 1) (MkY "2")
      d `shouldBe` (Just "MkX 1 => MkY \"2\"")

  describe "Record" $ do
    it "can should delta with fields" $ do
      let d = showDelta (Record 1 "x") (Record 2 "y")
      d `shouldBe` (Just "Record { intField: 1 => 2, stringField: \"x\" => \"y\" }")

-- `main` is here so that this module can be run from GHCi on its own.  It is
-- not needed for automatic spec discovery.
main :: IO ()
main = hspec spec
