{-# LANGUAGE NoMonomorphismRestriction #-}

module Main where

import Control.Monad
import Data.Function
import Data.List (find)
import qualified Data.Map.Strict as Map
import Polybar
import System.IO
import XMonad
import XMonad.Actions.CycleWS
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.FadeInactive
import XMonad.Hooks.ManageDocks
import XMonad.Layout.Named
import XMonad.Layout.NoBorders (smartBorders)
import XMonad.Layout.Reflect
import XMonad.Layout.ResizableTile
import XMonad.Layout.Spacing
import XMonad.Layout.ThreeColumns
import XMonad.Layout.WindowNavigation
import XMonad.Operations
import XMonad.StackSet as W
import XMonad.Util.CustomKeys
import XMonad.Util.EZConfig
import XMonad.Util.Run (spawnPipe)
import XmonadTheme

-- general definitions
terminalEmulator = "kitty"

launchRofi :: String -> String -> X ()
launchRofi combiModi cmd =
  let modi = "drun,ssh,window,filebrowser,combi"
   in spawn $
        "rofi -modi " <> modi
          <> " -combi-modi "
          <> combiModi
          <> " -show-icons -show "
          <> cmd

defaultCombiModi :: String
defaultCombiModi = "window,drun,filebrowser,ssh"

rofiCalc :: X ()
rofiCalc = spawn "rofi -modi calc -show calc -no-show-match -no-sort"

pickEmoji :: X ()
pickEmoji = spawn "rofi -modi emoji -show  emoji"

rofiHoogle :: X ()
rofiHoogle = spawn "rofi -modi hoogle -show hoogle"

launchEmacsClient :: X ()
launchEmacsClient =
  spawn "emacsclient -c"

launchers :: [((ButtonMask, KeySym), X ())]
launchers =
  [ ((mod4Mask, xK_p), launchRofi defaultCombiModi "combi")
  , ((mod4Mask, xK_Up), launchRofi "window" "window")
  , ((mod4Mask, xK_equal), rofiCalc)
  , ((mod4Mask, xK_slash), launchRofi "filebrowser,ssh" "filebrowser")
  , ((mod4Mask, xK_semicolon), pickEmoji)
  , ((mod4Mask, xK_o), rofiHoogle)
  , ((mod4Mask .|. shiftMask, xK_space), shiftNextScreen)
  --  , ((mod4Mask, xK_e),     launchEmacsClient)
  ]

customLayoutHook =
  let layouts' =
        threeColumn ||| reverseTall ||| tall ||| mirrorTall ||| full
      mkLayout f =
        f mainWindowCount incrementRatio mainWindowRatio
      incrementRatio = (3 / 100)
      mainWindowRatio = (1 / 2)
      mainWindowCount = 1
      threeColumn = mkLayout ThreeColMid
      tall = mkLayout Tall
      full = Full
      reverseTall = Mirror tall
      mirrorTall = reflectHoriz tall
   in spacingWithEdge 10 . smartBorders . avoidStruts $ layouts'

main = do
  polybarConfig <- defaultPolybarConfig
  xmonad . ewmh . stylishConfig defaultXMonadScheme $
    docks
      def
        { manageHook = manageDocks <+> manageHook def
        , layoutHook = customLayoutHook
        , logHook = fadeInactiveLogHook 0.9 <> polybarLogHook polybarConfig
        , modMask = mod4Mask
        , borderWidth = 2
        , terminal = terminalEmulator
        , normalBorderColor = "#333333"
        , focusedBorderColor = "#FFAA00"
        }
      `additionalKeys` launchers
