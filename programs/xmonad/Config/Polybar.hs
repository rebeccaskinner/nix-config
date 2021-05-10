{-# LANGUAGE RecordWildCards #-}
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

module Config.Polybar where

import           Codec.Binary.UTF8.String as Utf8
import           Config.Color.Type
import           Control.Monad.IO.Class
import qualified DBus                     as D
import qualified DBus.Client              as D
import           XMonad.Hooks.DynamicLog

type PolybarThemeType =
  '[ "focused_workspace_text"            :||: DefaultText1
   , "focused_workspace_background"      :||: DefaultText1
   , "visible_workspace_text"            :||: DefaultText2
   , "visible_workspace_background"      :||: DefaultText2
   , "hidden_workspace_text"             :||: DefaultText2
   , "hidden_workspace_background"       :||: DefaultText2
   , "empty_hidden_workspace_text"       :||: DefaultText2
   , "empty_hidden_workspace_background" :||: DefaultText2
   , "urgent_workspace_text"             :||: DefaultText1
   , "urgent_workspace_background"       :||: DefaultText1
   , "section_separator"                 :||: DefaultFG1
   , "workspace_separator"               :||: DefaultFG2
   ]

data PolybarTheme = PolybarTheme
  { _themeFocusedWorkspaceText           :: Color
  , _themeFocusedWorkspaceBackground     :: Color
  , _themeVisibleWorkspaceText           :: Color
  , _themeVisibleWorkspaceBackground     :: Color
  , _themeHiddenWorkspaceText            :: Color
  , _themeHiddenWorkspaceBackground      :: Color
  , _themeEmptyHiddenWorkspaceText       :: Color
  , _themeEmptyHiddenWorkspaceBackground :: Color
  , _themeUrgentWorkspaceText            :: Color
  , _themeUrgentWorkspaceBackground      :: Color
  , _themeSectionSeparator               :: Color
  , _themeWorkspaceSeparator             :: Color
  }

themeFromColorScheme colorScheme = PolybarTheme
  { _themeFocusedWorkspaceText            = getColor @"focused_workspace_text" colorScheme
  , _themeFocusedWorkspaceBackground      = getColor @"focused_workspace_background" colorScheme
  , _themeVisibleWorkspaceText            = getColor @"visible_workspace_text" colorScheme
  , _themeVisibleWorkspaceBackground      = getColor @"visible_workspace_background" colorScheme
  , _themeHiddenWorkspaceText             = getColor @"hidden_workspace_text" colorScheme
  , _themeHiddenWorkspaceBackground       = getColor @"hidden_workspace_background" colorScheme
  , _themeEmptyHiddenWorkspaceText        = getColor @"empty_hidden_workspace_text" colorScheme
  , _themeEmptyHiddenWorkspaceBackground  = getColor @"empty_hidden_workspace_background" colorScheme
  , _themeUrgentWorkspaceText             = getColor @"urgent_workspace_text" colorScheme
  , _themeUrgentWorkspaceBackground       = getColor @"urgent_workspace_background" colorScheme
  , _themeSectionSeparator                = getColor @"section_separator" colorScheme
  , _themeWorkspaceSeparator              = getColor @"workspace_separator" colorScheme
  }

data PolybarConfig = PolybarConfig
  { client :: D.Client
  , theme  :: PolybarTheme
  }

dbusClient :: IO D.Client
dbusClient =  do
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
    PolybarConfig dbus PolybarTheme{..} = cfg
    objPath = D.objectPath_ "/org/xmonad/Log"
    ifaceName = D.interfaceName_ "org.xmonad.Log"
    memberName = D.memberName_ "Update"
    signal = D.signal objPath ifaceName memberName
    body = pure . D.toVariant . Utf8.decodeString $ msg
  in D.emit dbus $ signal { D.signalBody = body }
