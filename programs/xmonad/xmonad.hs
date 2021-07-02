{-# LANGUAGE NoMonomorphismRestriction #-}
module Main where
import           Control.Monad
import           Data.Function
import           Data.List                      (find)
import qualified Data.Map.Strict                as Map
import           Polybar
import           System.IO
import           XMonad
import           XMonad.Hooks.DynamicLog
import           XMonad.Hooks.FadeInactive
import           XMonad.Hooks.ManageDocks
import           XMonad.Layout.Named
import           XMonad.Layout.NoBorders        (smartBorders)
import           XMonad.Layout.Reflect
import           XMonad.Layout.ResizableTile
import           XMonad.Layout.WindowNavigation
import           XMonad.Operations
import           XMonad.StackSet                as W
import           XMonad.Util.CustomKeys
import           XMonad.Util.EZConfig
import           XMonad.Util.Run                (spawnPipe)
import XMonad.Hooks.EwmhDesktops
import           XmonadTheme

-- general definitions
terminalEmulator = "kitty"

launchRofi :: String -> X ()
launchRofi cmd =
  spawn $ "rofi -modi drun,ssh,window,file-browser -show-icons -show " <> cmd

launchEmacsClient :: X ()
launchEmacsClient =
  spawn "emacsclient -c"

launchers :: [((ButtonMask, KeySym),X ())]
launchers =
  [ ((mod4Mask, xK_e), launchEmacsClient)
  , ((mod4Mask, xK_p), launchRofi "drun")
  , ((mod4Mask, xK_slash), launchRofi "file-browser")
  ]

main = do
  polybarConfig <- defaultPolybarConfig
  xmonad . ewmh . stylishConfig defaultXMonadScheme $ docks def
    { manageHook = manageDocks <+> manageHook def
    , layoutHook = smartBorders . avoidStruts $ layoutHook def
    , logHook = fadeInactiveLogHook 0.9 <> polybarLogHook polybarConfig
    , modMask = mod4Mask
    , borderWidth = 2
    , terminal = terminalEmulator
    , normalBorderColor = "#333333"
    , focusedBorderColor = "#FFAA00"
    } `additionalKeys` launchers
