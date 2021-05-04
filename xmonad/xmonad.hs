{-# LANGUAGE NoMonomorphismRestriction #-}
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
import Data.List (find)
import System.IO
import Control.Monad
import Data.Function
import qualified Data.Map.Strict as Map

-- general definitions
terminalEmulator = "kitty"

launchEmacsClient :: X ()
launchEmacsClient =
  spawn "emacsclient -c"

launchers :: [((ButtonMask, KeySym),X ())]
launchers =
  [((mod4Mask, xK_e), launchEmacsClient)]


main = do
  xmproc <- spawnPipe "xmobar"
  xmonad $ docks def
    { manageHook = manageDocks <+> manageHook defaultConfig
    , layoutHook = smartBorders . avoidStruts $ layoutHook defaultConfig
    , logHook = dynamicLogWithPP xmobarPP
      { ppOutput = hPutStrLn xmproc
      , ppTitle = xmobarColor "#D2B6FA" "" . shorten 50
      }
    , modMask = mod4Mask
    , borderWidth = 2
    , terminal = terminalEmulator
    , normalBorderColor = "#333333"
    , focusedBorderColor = "#FFAA00"
    } `additionalKeys` launchers
