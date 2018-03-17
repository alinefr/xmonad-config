{-# LANGUAGE OverloadedStrings #-}

import XMonad
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Util.Cursor
import XMonad.Util.Run(spawnPipe)
import XMonad.Util.EZConfig(additionalKeys, additionalKeysP)
import XMonad.Util.SpawnOnce
import DBus
import DBus.Client
import System.IO

spotifyCtrl :: Client -> MemberName -> X ()
spotifyCtrl client command = liftIO $ do
  call_ client
    (methodCall "/org/mpris/MediaPlayer2" "org.mpris.MediaPlayer2.Player" command) {
      methodCallDestination = Just "org.mpris.MediaPlayer2.spotify" }
  return ( )

myManageHook = composeAll
    [ className =? "Gimp" --> doFloat
    , className =? "onboard" --> doIgnore
    , className =? "trayer" --> doIgnore
    ]

myStatusBar = "xmobar"

myStartupHook = do
  setDefaultCursor xC_left_ptr
  spawn "trayer --edge top --align right --SetDockType true --expand true --width 10 --transparent true --tint 0x191970 --height 12 --SetPartialStrut true"
  spawn "xsetroot -solid midnightblue"
  spawn "dhcpcd-gtk"
  spawn "parcellite"
  spawn "xscreensaver -no-splash -lock"


myKeys = [ ((mod4Mask .|. shiftMask, xK_z), spawn "xscreensaver-command -lock")
         , ((mod4Mask .|. shiftMask, xK_k), spawn "onboard")
         , ((controlMask, xK_Print), spawn "sleep 0.2; scrot -s")
         , ((0, xK_Print), spawn "scrot")
         ]

myKeysP dbusClient = [ ("<XF86AudioMute>", spawn "amixer -q sset Master toggle")
                     , ("<XF86AudioRaiseVolume>", spawn "amixer -q sset Master 5+ unmute")
                     , ("<XF86AudioLowerVolume>", spawn "amixer -q sset Master 5- unmute")
                     , ("<XF86AudioPlay>", spotifyCtrl dbusClient "PlayPause")
                     , ("<XF86AudioNext>", spotifyCtrl dbusClient "Next")
                     , ("<XF86AudioPrev>", spotifyCtrl dbusClient "Previous")
                     ]

main = do
    xmproc <- spawnPipe myStatusBar
    client <- connectSession
    xmonad $ def
        { manageHook = manageDocks <+> myManageHook -- make sure to include myManageHook definition from above
                        <+> manageHook def
        , layoutHook = avoidStruts  $  layoutHook def
        , handleEventHook = handleEventHook def <+> docksEventHook
        , logHook = dynamicLogWithPP xmobarPP
                        { ppOutput = hPutStrLn xmproc
                        , ppTitle = xmobarColor "green" "" . shorten 50
                        }
        , terminal = "xfce4-terminal -e tmux"
        , modMask = mod4Mask     -- Rebind Mod to the Windows key
        , startupHook = myStartupHook
        } `additionalKeys` myKeys
          `additionalKeysP` myKeysP (client)
