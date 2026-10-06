{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module ColorType (
  Color (..),
  IsColor (..),
  toHex,
  ColorScheme (..),
  (:|:),
  (:||:),
  (.==),
  Theme' (..),
  Theme,
  DefaultTheme,
  DefaultThemeType (..),
  VerifyColorScheme (..),
  unsafeVerifyColorScheme,
  getColor,
) where

import Control.Applicative
import qualified Data.ByteString.Char8 as BS
import qualified Data.Map.Strict as Map
import Data.Maybe (fromJust)
import Data.Proxy
import Data.Word
import GHC.TypeLits
import Text.Printf

infixr 6 :|:
infixr 6 :||:
infixr 5 :.:

data a :|: b = a :|: b
type a :||: b = a ':|: b

infixr 6 .==
(.==) :: forall color. IsColor color => BS.ByteString -> color -> (BS.ByteString, SomeColor)
name .== color = (name, SomeColor color)

class IsColor a where
  toColor :: a -> Color

data Color = RGBColor {red :: !Word8, green :: !Word8, blue :: !Word8}
  deriving (Eq, Show)

instance IsColor Color where
  toColor = id

toHex :: IsColor a => a -> String
toHex color =
  case toColor color of
    RGBColor{..} ->
      printf "#%x%x%x" red green blue

symBS :: forall n. KnownSymbol n => BS.ByteString
symBS = BS.pack . symbolVal $ Proxy @n

data SomeColor = forall a. IsColor a => SomeColor a

instance IsColor SomeColor where
  toColor (SomeColor a) = toColor a

newtype ColorScheme theme = ColorScheme {getColorScheme :: Map.Map BS.ByteString SomeColor}

data Theme' a = EmptyTheme | a :.: (Theme' a)
type Theme = Theme' (Symbol :|: DefaultThemeType)

class HasColor goal (a :: Theme)
instance HasColor goal (goal :||: defaultTheme ':.: rest)
instance {-# OVERLAPPABLE #-} (HasColor goal rest) => HasColor goal (color :||: defaultTheme ':.: rest)

class VerifyColorScheme a where
  verifyColorScheme :: Map.Map BS.ByteString SomeColor -> Maybe (ColorScheme a)

instance VerifyColorScheme 'EmptyTheme where
  verifyColorScheme = const . Just . ColorScheme $ Map.empty

instance
  ( KnownSymbol color
  , KnownSymbol fallbackName
  , VerifyColorScheme colors
  , fallbackName ~ DefaultThemeSymbol fallback
  ) =>
  VerifyColorScheme ((color :||: fallback) ':.: colors)
  where
  verifyColorScheme m = do
    let colorName = symBS @color
        fallbackName = symBS @fallbackName
    color <- Map.lookup colorName m <|> Map.lookup fallbackName m
    ColorScheme theme <- verifyColorScheme @colors m
    pure . ColorScheme $ Map.insert colorName (SomeColor color) theme

unsafeVerifyColorScheme :: (VerifyColorScheme theme) => Map.Map BS.ByteString SomeColor -> ColorScheme theme
unsafeVerifyColorScheme =
  fromJust . verifyColorScheme

class SatisfiesTheme theme toSatisfy
instance SatisfiesTheme theme 'EmptyTheme
instance (HasColor color theme, SatisfiesTheme theme rest) => SatisfiesTheme theme (color :||: def ':.: rest)

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

type DefaultTheme =
  "default_background_1" :||: 'DefaultBG1
    ':.: "default_background_2" :||: 'DefaultBG2
    ':.: "default_background_3" :||: 'DefaultBG3
    ':.: "default_foreground_1" :||: 'DefaultFG1
    ':.: "default_foreground_2" :||: 'DefaultFG2
    ':.: "default_foreground_3" :||: 'DefaultFG3
    ':.: "default_text_1" :||: 'DefaultText1
    ':.: "default_text_2" :||: 'DefaultText2
    ':.: 'EmptyTheme

type family DefaultThemeSymbol (defaultField :: DefaultThemeType) :: Symbol where
  DefaultThemeSymbol 'DefaultBG1 = "default_background_1"
  DefaultThemeSymbol 'DefaultBG2 = "default_background_2"
  DefaultThemeSymbol 'DefaultBG3 = "default_background_3"
  DefaultThemeSymbol 'DefaultFG1 = "default_foreground_1"
  DefaultThemeSymbol 'DefaultFG2 = "default_foreground_2"
  DefaultThemeSymbol 'DefaultFG3 = "default_foreground_3"
  DefaultThemeSymbol 'DefaultText1 = "default_text_1"
  DefaultThemeSymbol 'DefaultText2 = "default_text_2"

getColor :: forall element theme. (KnownSymbol element, HasColor element theme) => ColorScheme theme -> Color
getColor (ColorScheme scheme) =
  toColor $ scheme Map.! (symBS @element)
