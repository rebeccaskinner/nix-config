{-# LANGUAGE RecordWildCards #-}
module Config.Polybar where

import Codec.Binary.UTF8.String as Utf8
import qualified DBus as D
import qualified DBus.Client as D
import XMonad.Hooks.DynamicLog
import Control.Monad.IO.Class

data PolybarTheme = PolybarTheme
  {
  } deriving (Show)

data PolybarConfig = PolybarConfig
  { client :: D.Client
  , theme :: PolybarTheme
  }

newPolybarConfig :: MonadIO m => PolybarTheme -> m PolybarConfig
newPolybarConfig theme =
  (`PolybarConfig` theme) <$> dbusClient

dbusClient :: MonadIO m => m D.Client
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
