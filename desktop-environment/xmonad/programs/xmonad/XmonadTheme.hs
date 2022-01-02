{-# LANGUAGE AllowAmbiguousTypes   #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE ImportQualifiedPost   #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE PolyKinds             #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE UndecidableInstances  #-}
module XmonadTheme where

import qualified Data.Map.Strict as Map
import qualified ColorX11 as X11
import ColorType
import XMonad.Core

type XMonadTheme
  =   "active_window_border"   :||: DefaultFG1
  :.: "inactive_window_border" :||: DefaultFG2
  :.: EmptyTheme

defaultXMonadScheme :: ColorScheme XMonadTheme
defaultXMonadScheme = unsafeVerifyColorScheme @XMonadTheme . Map.fromList $
  [ "active_window_border" .== X11.Plum
  , "inactive_window_border" .== X11.RebeccaPurple
  ]

stylishConfig :: ColorScheme XMonadTheme -> XConfig a -> XConfig a
stylishConfig scheme cfg = cfg
  { normalBorderColor  = toHex $ getColor @"inactive_window_border" scheme
  , focusedBorderColor = toHex $ getColor @"active_window_border" scheme
  }
