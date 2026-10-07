module Main (main) where

import ColorType
import ColorX11 qualified as X11
import Test.Hspec

main :: IO ()
main = hspec $ do
  describe "toHex" $ do
    it "renders an X11 color as a hex string" $
      toHex X11.Plum `shouldBe` "#dda0dd"
