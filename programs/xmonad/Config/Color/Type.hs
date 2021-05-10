{-# LANGUAGE AllowAmbiguousTypes   #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE PolyKinds             #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE UndecidableInstances  #-}

module Config.Color.Type
  ( Color(..)
  , toHex
  ) where

import           Control.Applicative
import           Control.Monad.Reader
import qualified Data.ByteString.Char8 as BS
import           Data.Kind
import qualified Data.Map.Strict       as Map
import           Data.Proxy
import           Data.Void
import           Data.Word
import           GHC.TypeLits
import           Text.Printf

class IsColor a where
  isColor :: Color

data Color
  = RGBColor { red :: !Word8, green :: !Word8, blue :: !Word8 }
  deriving (Eq, Show)

toHex :: Color -> String
toHex RGBColor{..} =
  printf "#%x%x%x" red green blue

symBS :: forall n. KnownSymbol n => BS.ByteString
symBS = BS.pack . symbolVal $ Proxy @n

newtype ColorScheme theme =
  ColorScheme { getColorScheme :: Map.Map BS.ByteString Color } deriving Show

class UnifyColorScheme a where
  unifyColorScheme :: Map.Map BS.ByteString Color -> Maybe (ColorScheme a)

instance UnifyColorScheme '[] where
 unifyColorScheme m = Just . ColorScheme $ Map.empty

instance
  ( KnownSymbol color
  , KnownSymbol fallbackColor
  , fallbackColor ~ DefaultThemeSymbol fallback
  , UnifyColorScheme colors) => UnifyColorScheme ('(color, fallback) ': colors) where
  unifyColorScheme m = do
    let
      colorName = symBS @color
      fallbackName = symBS @fallbackColor
    color <- Map.lookup colorName m <|> Map.lookup fallbackName m
    ColorScheme theme <- unifyColorScheme @colors m
    pure . ColorScheme $ Map.insert colorName color theme

data DefaultColorTheme = DefaultColorTheme
  { defaultBG1           :: !Color
  , defaultBG2           :: !Color
  , defaultBG3           :: !Color
  , defaultFG1           :: !Color
  , defaultFG2           :: !Color
  , defaultFG3           :: !Color
  , defaultTextPrimary   :: !Color
  , defaultTextSecondary :: !Color
  } deriving (Eq, Show)

data DefaultThemeType
  = DefaultBG1
  | DefaultBG2
  | DefaultBG3
  | DefaultFG1
  | DefaultFG2
  | DefaultFG3
  | DefaultText1
  | DefaultText2
  deriving (Eq, Show)

type family DefaultThemeSymbol (defaultField :: DefaultThemeType) :: Symbol where
  DefaultThemeSymbol DefaultBG1 = "default_background_1"
  DefaultThemeSymbol DefaultBG2 = "default_background_2"
  DefaultThemeSymbol DefaultBG3 = "default_background_3"
  DefaultThemeSymbol DefaultFG1 = "default_foreground_1"
  DefaultThemeSymbol DefaultFG2 = "default_foreground_2"
  DefaultThemeSymbol DefaultFG3 = "default_foreground_3"
  DefaultThemeSymbol DefaultText1 = "default_text_1"
  DefaultThemeSymbol DefaultText2 = "default_text_2"

type family FindColor' colorName (themes :: [(Symbol, DefaultThemeType)]) where
   FindColor' name '[] = False
   FindColor' name ('(name, def) ': rest) = True
   FindColor' name ('(name', def) ': rest) = FindColor' name rest

type family HasColor colorName themes where
  HasColor colorName themes = (True ~ FindColor' colorName themes)

getColor
  :: forall element theme.
  ( KnownSymbol element
  , HasColor element theme)
  => ColorScheme theme
  -> Color
getColor (ColorScheme scheme) =
  scheme Map.! (symBS @element)

testMap :: Map.Map BS.ByteString Color
testMap = Map.fromList $
  [("foo", RGBColor 0 0 0)
  ,("bar", RGBColor 1 1 1)
  ,("default_background_3", RGBColor 2 2 2)
  ]

scheme :: ColorScheme ['("foo", DefaultBG1),'("bar", DefaultBG2),'("baz", DefaultBG3)]
-- scheme :: ColorScheme [ "foo" :||: DefaultBG1
--                       , "bar" :||: DefaultBG2
--                       , "baz" :||: DefaultBG3]
scheme =
  let (Just u) = unifyColorScheme $ testMap
  in u

data a :||: b
-- data (a :: Symbol) :||: (b :: DefaultThemeType)
-- type Theme (a :: Symbol) (b :: DefaultThemeType) = '[a :||: b]

-- type PolybarTheme =
--   '[ "foo" :||: DefaultBG1
--    , "bar" :||: DefaultBG2
--    , "baz" :||: DefaultBG3
--    ]


type PolybarTheme =
  '[ '("foo", DefaultBG1)
   , '("bar", DefaultBG2)
   , '("baz", DefaultBG3)
   ]

parseScheme = unifyColorScheme @PolybarTheme

runTest scheme =
  let fooColor = getColor @"foo" scheme
      barColor = getColor @"bar" scheme
      bazColor = getColor @"baz" scheme
  in (fooColor, barColor, bazColor)
