import XMonad

import XMonad.Config.Gnome
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
-- import XMonad.Util.Paste
import XMonad.Util.Run(spawnPipe)
import XMonad.Util.EZConfig
import XMonad.Actions.PhysicalScreens
import XMonad.Hooks.DynamicLog
-- import qualified XMonad.StackSet as W
import System.IO


main = do
    h <- spawnPipe "/usr/local/bin/xmobar"
    xmonad $ defaultConfig
        { terminal = "/opt/local/bin/urxvt"
        , focusFollowsMouse = False
        , focusedBorderColor = "#2F95ED"
        , borderWidth = 5
        , manageHook = manageDocks <+> myManageHook <+> manageHook defaultConfig
        , layoutHook = avoidStruts  $  layoutHook defaultConfig
        , logHook = dynamicLogWithPP $ defaultPP
            { ppSep = " | "
            , ppTitle = xmobarColor "green" ""
            , ppOutput = hPutStrLn h
            , ppCurrent = xmobarColor "yellow" ""
            , ppUrgent = xmobarColor "red" "yellow"
            }
        -- , workspaces = myWorkspaces
        }
        `additionalKeys` myKeys
        --`additionalKeysP` myKeysP

myKeys =
    [ ((mod1Mask .|. shiftMask, xK_s), spawn "/opt/local/bin/urxvt -pe spdev")
    , ((mod1Mask .|. shiftMask, xK_d), spawn "/opt/local/bin/urxvt -pe dev2")
    , ((mod1Mask .|. shiftMask, xK_f), spawn "/opt/local/bin/urxvt -pe s4hb-pkr-dev1")
    -- , ((mod1Mask, xK_v), pasteSelection)
    ]
    ++
    [ --
      -- mod-{w,e,r}, Switch to physical/Xinerama screens 1, 2, or 3
      -- mod-shift-{w,e,r}, Move client to screen 1, 2, or 3
      --
      ((mod1Mask .|. mask, key), f sc)
        | (key, sc) <- zip [xK_w, xK_e, xK_r] [0..]
        , (f, mask) <- [(viewScreen, 0), (sendToScreen, shiftMask)]
    ]

-- myKeysP =
--     [ --
--       -- Use non-greedy view
--       --
--       (otherModMasks ++ "M-" ++ [key], action tag)
--          | (tag, key) <- zip myWorkspaces "123456789"
--          , (otherModMasks, action) <- [ ("", windows . W.view)
--                                       , ("S-", windows . W.shift)]
--     ]

-- myWorkspaces = ["one","two","three","foo","bar","baz","lambda","no","more"]

myManageHook = composeAll
    [ resource =? "Do" --> doIgnore
    ]

