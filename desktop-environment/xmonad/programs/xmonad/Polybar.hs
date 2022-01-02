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

module Polybar where

import qualified Codec.Binary.UTF8.String as Utf8
import           ColorType
import qualified ColorX11 as X11
import           Control.Monad.IO.Class
import Data.Aeson
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import qualified DBus                     as D
import qualified DBus.Client              as D
import qualified Data.ByteString.Lazy     as BL
import qualified Data.ByteString.Char8 as BS
import           XMonad.Hooks.DynamicLog
import qualified Data.Map.Strict as Map

type PolybarTheme
  =   "focused_workspace_text"            :||: DefaultText1
  :.: "focused_workspace_background"      :||: DefaultBG1
  :.: "visible_workspace_text"            :||: DefaultText2
  :.: "visible_workspace_background"      :||: DefaultBG1
  :.: "hidden_workspace_text"             :||: DefaultText2
  :.: "hidden_workspace_background"       :||: DefaultBG1
  :.: "empty_hidden_workspace_text"       :||: DefaultText2
  :.: "empty_hidden_workspace_background" :||: DefaultBG1
  :.: "urgent_workspace_text"             :||: DefaultText1
  :.: "urgent_workspace_background"       :||: DefaultBG1
  :.: "section_separator"                 :||: DefaultFG1
  :.: "workspace_separator"               :||: DefaultFG2
  :.: DefaultTheme

data PolybarScheme = PolybarScheme
  { _themeFocusedWorkspaceText           :: String
  , _themeFocusedWorkspaceBackground     :: String
  , _themeVisibleWorkspaceText           :: String
  , _themeVisibleWorkspaceBackground     :: String
  , _themeHiddenWorkspaceText            :: String
  , _themeHiddenWorkspaceBackground      :: String
  , _themeEmptyHiddenWorkspaceText       :: String
  , _themeEmptyHiddenWorkspaceBackground :: String
  , _themeUrgentWorkspaceText            :: String
  , _themeUrgentWorkspaceBackground      :: String
  , _themeSectionSeparator               :: String
  , _themeWorkspaceSeparator             :: String
  }

polybarScheme :: ColorScheme PolybarTheme -> PolybarScheme
polybarScheme colorScheme = PolybarScheme
  { _themeFocusedWorkspaceText            = toHex $ getColor @"focused_workspace_text" colorScheme
  , _themeFocusedWorkspaceBackground      = toHex $ getColor @"focused_workspace_background" colorScheme
  , _themeVisibleWorkspaceText            = toHex $ getColor @"visible_workspace_text" colorScheme
  , _themeVisibleWorkspaceBackground      = toHex $ getColor @"visible_workspace_background" colorScheme
  , _themeHiddenWorkspaceText             = toHex $ getColor @"hidden_workspace_text" colorScheme
  , _themeHiddenWorkspaceBackground       = toHex $ getColor @"hidden_workspace_background" colorScheme
  , _themeEmptyHiddenWorkspaceText        = toHex $ getColor @"empty_hidden_workspace_text" colorScheme
  , _themeEmptyHiddenWorkspaceBackground  = toHex $ getColor @"empty_hidden_workspace_background" colorScheme
  , _themeUrgentWorkspaceText             = toHex $ getColor @"urgent_workspace_text" colorScheme
  , _themeUrgentWorkspaceBackground       = toHex $ getColor @"urgent_workspace_background" colorScheme
  , _themeSectionSeparator                = toHex $ getColor @"section_separator" colorScheme
  , _themeWorkspaceSeparator              = toHex $ getColor @"workspace_separator" colorScheme
  }

defaultColorScheme :: ColorScheme PolybarTheme
defaultColorScheme = unsafeVerifyColorScheme @PolybarTheme . Map.fromList $
  [ "focused_workspace_text"            .== X11.Plum
  , "focused_workspace_background"      .== X11.RebeccaPurple
  , "visible_workspace_text"            .== X11.Plum
  , "visible_workspace_background"      .== X11.RebeccaPurple
  , "hidden_workspace_text"             .== RGBColor 0x88 0x55 0xBB
  , "hidden_workspace_background"       .== X11.RebeccaPurple
  , "empty_hidden_workspace_text"       .== X11.WebPurple
  , "empty_hidden_workspace_background" .== X11.RebeccaPurple
  , "urgent_workspace_text"             .== X11.Magenta
  , "urgent_workspace_background"       .== X11.RebeccaPurple
  , "section_separator"                 .== X11.MediumOrchid
  , "workspace_separator"               .== X11.MediumOrchid
  , "default_background_1"              .== RGBColor 0x26 0x26 0x26
  , "default_background_2"              .== RGBColor 0x26 0x26 0x26
  , "default_background_3"              .== RGBColor 0x26 0x26 0x26
  , "default_foreground_1"              .== X11.Plum
  , "default_foreground_2"              .== X11.Plum
  , "default_foreground_3"              .== X11.Plum
  , "default_text_1"                    .== X11.Magenta
  , "default_text_2"                    .== X11.Magenta
  ]

themedPP :: PolybarConfig -> PP
themedPP config =
  let
    PolybarScheme{..} = theme config
    fmt color =
      let
        wrapperFront = "%{F"<>color<>"}"
      in \s -> wrapperFront <> s <> "%{F-}"

    underline s = "%{U}"<>s<>"%{U-}"

    truncateTo n s
      | length s <= (n - 3) = s
      | otherwise = take (n - 3) s <> "..."

  in def
     { ppOutput           = dbusOutput config
     , ppCurrent          = \s -> fmt _themeFocusedWorkspaceText $ "["<>s<>"]"
     , ppVisible          = fmt _themeVisibleWorkspaceText
     , ppHidden           = fmt _themeHiddenWorkspaceText
     , ppHiddenNoWindows  = const ""
     , ppOrder            = \(workspace:layout:title:rest) -> title:workspace:(rest <> pure layout)
     , ppUrgent           = fmt _themeUrgentWorkspaceText
     , ppTitle            = \title ->
                              case title of
                                "" -> fmt _themeFocusedWorkspaceText "<Desktop>"
                                title' -> fmt _themeFocusedWorkspaceText $ truncateTo (maxTitleLength config) title'
     , ppLayout           = fmt _themeFocusedWorkspaceText
     , ppSep              = fmt _themeSectionSeparator " · "
     }

data PolybarConfig = PolybarConfig
  { dbusClient :: D.Client
  , theme  :: PolybarScheme
  , maxTitleLength :: Int
  }

mkDbusClient :: IO D.Client
mkDbusClient =  do
  dbus <- D.connectSession
  D.requestName dbus xmonadBusName xmonadRequestNameFlags
  pure dbus
  where
    xmonadBusName :: D.BusName
    xmonadBusName = D.busName_ "org.xmonad.log"

    xmonadRequestNameFlags :: [D.RequestNameFlag]
    xmonadRequestNameFlags =
      [ D.nameAllowReplacement
      , D.nameReplaceExisting
      , D.nameDoNotQueue
      ]

dbusOutput :: PolybarConfig -> String -> IO ()
dbusOutput cfg msg =
  let
    PolybarConfig
      { dbusClient = dbus
      , theme =  PolybarScheme{..}
      , maxTitleLength = _
      }= cfg
    objPath = D.objectPath_ "/org/xmonad/Log"
    ifaceName = D.interfaceName_ "org.xmonad.Log"
    memberName = D.memberName_ "Update"
    signal = D.signal objPath ifaceName memberName
    body = pure . D.toVariant . Utf8.decodeString $ msg
  in D.emit dbus $ signal { D.signalBody = body }

defaultPolybarConfig :: IO PolybarConfig
defaultPolybarConfig = do
  client <- mkDbusClient
  pure $ PolybarConfig { dbusClient = client
                       , theme = polybarScheme defaultColorScheme
                       , maxTitleLength = 25
                       }

polybarLogHook = dynamicLogWithPP . themedPP
