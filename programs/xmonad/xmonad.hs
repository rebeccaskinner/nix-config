{-# LANGUAGE NoMonomorphismRestriction #-}
module Main where
import XMonad
import XMonad.Operations
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Util.Run(spawnPipe)
import XMonad.Util.EZConfig
import XMonad.Layout.Named
import XMonad.Layout.Reflect
import XMonad.Layout.ResizableTile
import XMonad.Layout.WindowNavigation
import XMonad.Layout.NoBorders (smartBorders)
import XMonad.Util.CustomKeys
import XMonad.StackSet as W
import XMonad.Hooks.FadeInactive
import Data.List (find)
import System.IO
import Control.Monad
import Data.Function
import qualified Data.Map.Strict as Map
import Polybar

-- general definitions
terminalEmulator = "kitty"

launchRofi :: X ()
launchRofi =
  spawn "rofi -modi drun,ssh,window -show drun -show-icons"

launchEmacsClient :: X ()
launchEmacsClient =
  spawn "emacsclient -c"

launchers :: [((ButtonMask, KeySym),X ())]
launchers =
  [((mod4Mask, xK_e), launchEmacsClient)
  ,((mod4Mask, xK_p), launchRofi)
  ]

main = do
  polybarConfig <- defaultPolybarConfig
  xmonad $ docks def
    { manageHook = manageDocks <+> manageHook def
    , layoutHook = smartBorders . avoidStruts $ layoutHook def
    , logHook = fadeInactiveLogHook 0.9 <> polybarLogHook polybarConfig
    , modMask = mod4Mask
    , borderWidth = 2
    , terminal = terminalEmulator
    , normalBorderColor = "#333333"
    , focusedBorderColor = "#FFAA00"
    } `additionalKeys` launchers
