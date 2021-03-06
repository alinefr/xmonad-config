{-# LANGUAGE OverloadedStrings #-}

import XMonad
import XMonad.Actions.CycleWS
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.SetWMName
import XMonad.Layout.IndependentScreens
import XMonad.Prompt
import XMonad.Prompt.ConfirmPrompt
import XMonad.Prompt.Pass
import XMonad.Prompt.RunOrRaise
import XMonad.Util.Cursor
import XMonad.Util.Paste
import XMonad.Util.Run(spawnPipe)
import XMonad.Util.EZConfig(additionalKeys, additionalKeysP)
import XMonad.Util.SpawnOnce
import System.Exit
import System.IO
import qualified XMonad.StackSet as W

quitXmonad :: X ()
quitXmonad = io exitSuccess

myManageHook = composeAll
    [ className =? "Gimp" --> doFloat
    , className =? "trayer" --> doIgnore
    ]

myWorkspaces = ["1","2","3","4","5","6","7","8","9"]

myStatusBar = "xmobar"

myStartupHook = do
  setDefaultCursor xC_left_ptr
  setWMName "LG3D"
  spawn "~/bin/check-hdmi.hs"
  setWMName "LG3D"
  spawn "trayer --edge top --align right --SetDockType true --expand true --width 10 --transparent true --tint 0x191970 --height 12 --SetPartialStrut true"
  spawn "xloadimage -onroot -fullscreen ~/Images/xmonad.jpg"
  spawn "dhcpcd-gtk"
  spawn "parcellite"
  spawn "blueman-applet"
  spawn "start-pulseaudio-x11"
  spawn "xrandr --output HDMI-1 --mode 1920x1080 --output eDP-1 --mode 1366x768 --rate 60.11 --right-of HDMI-1"
  spawn "xscreensaver -no-splash"

myXPConfig = defaultXPConfig {
    font = "xft:inconsolata:size=11:antialias=true:hinting=true:hintstyle=hintfull"
      , bgColor = "#171717"
      , fgColor = "#ff7701"
      , bgHLight = "#171717"
      , fgHLight = "#00aa4a"
      , promptBorderWidth = 0
      , height = 16
      , historySize = 512
      , historyFilter = deleteConsecutive
}

myKeys = [ ((mod4Mask .|. shiftMask, xK_z), spawn "xscreensaver-command -lock")
         , ((mod4Mask .|. shiftMask, xK_k), spawn "onboard")
         , ((mod4Mask, xK_p), runOrRaisePrompt myXPConfig)
         , ((mod4Mask .|. shiftMask, xK_o), shiftNextScreen)
         , ((mod4Mask .|. mod1Mask, xK_o), swapNextScreen)
         , ((controlMask, xK_Print), spawn "sleep 0.2; scrot -s")
         , ((0, xK_Print), spawn "scrot")
         , ((mod4Mask .|. shiftMask, xK_q), confirmPrompt myXPConfig "Quit XMonad" $ io exitSuccess)
         , ((mod4Mask, xK_n), passPrompt myXPConfig)
         , ((mod4Mask .|. controlMask, xK_n), passGeneratePrompt myXPConfig)
         , ((mod4Mask .|. controlMask  .|. shiftMask, xK_n), passRemovePrompt myXPConfig)
         , ((0, xK_Insert), pasteSelection)
         , ((0, 0x1008FF2A), confirmPrompt myXPConfig "Suspend" $ spawn "dbus-send --system --print-reply --dest=org.freedesktop.ConsoleKit /org/freedesktop/ConsoleKit/Manager org.freedesktop.ConsoleKit.Manager.Suspend boolean:true")
         ]++
         [((m .|. mod4Mask, k), windows $ onCurrentScreen f i)
            | (i, k) <- zip (workspaces' def) [xK_1 .. xK_9]
            , (f, m) <- [(W.greedyView, 0), (W.shift, shiftMask)]
         ]

myKeysP = [ ("<XF86AudioMute>", spawn "amixer -q sset Master toggle")
          , ("<XF86AudioLowerVolume>", spawn "amixer -q sset Master 5- unmute")
          , ("<XF86AudioRaiseVolume>", spawn "amixer -q sset Master 5+ unmute")
          ]++
          [ (otherModMasks ++ "M-" ++ [key], action tag)
            | (tag, key)  <- zip myWorkspaces "123456789"
            , (otherModMasks, action) <- [ ("", windows . W.view) -- was W.greedyView
                                                , ("S-", windows . W.shift)]
          ]

main = do
    xmproc <- spawnPipe myStatusBar
    xmonad $ def
        { manageHook = manageDocks <+> myManageHook <+> manageHook def
        , layoutHook = avoidStruts  $  layoutHook def
        , workspaces = myWorkspaces
        , handleEventHook = handleEventHook def <+> docksEventHook <+> fullscreenEventHook
        , logHook = dynamicLogWithPP xmobarPP
                        { ppOutput = hPutStrLn xmproc
                        , ppTitle = xmobarColor "green" "" . shorten 50
                        }
        , terminal = "xfce4-terminal -e tmux"
        , modMask = mod4Mask
        , startupHook = myStartupHook
        } `additionalKeys` myKeys
          `additionalKeysP` myKeysP
