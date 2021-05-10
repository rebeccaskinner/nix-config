module Config.Color where
import Data.Bits
import Data.Word
import Text.Printf
import qualified Data.ByteString.Char8 as BS

data Color
  = RGBColor (!Word8, !Word8, !Word8)
  deriving (Eq, Show)

toHex :: Color -> String
toHex (RGBColor (r,g,b)) =
  printf "#%x%x%x" r g b
