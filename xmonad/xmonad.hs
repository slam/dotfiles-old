import XMonad

-- import XMonad.Config.Gnome
import XMonad.Hooks.ManageDocks
import XMonad.Util.Run(spawnPipe)
import XMonad.Util.EZConfig
import XMonad.Actions.PhysicalScreens
import XMonad.Actions.DynamicWorkspaces
import XMonad.Hooks.DynamicLog
import qualified XMonad.StackSet as W
import System.IO

import XMonad.Actions.TopicSpace as TS
import XMonad.Prompt
import XMonad.Prompt.Shell
import XMonad.Prompt.Workspace
import qualified Data.Map as M
import Data.List

--
-- Stole lots of ideas on TopicSpace configuation from:
--
-- http://github.com/aniederl/config-xmonad/blob/master/xmonad.hs
--
myTopics :: [Topic]
myTopics =
    [ "dashboard"
    , "fblogs"
    , "xmonad"
    ]

myTopicConfig :: TopicConfig
myTopicConfig = TopicConfig
    { topicDirs = M.fromList $
        [ ("dashboard", "~/Desktop")
        , ("xmonad", "~/.xmonad")
        ]
    , defaultTopicAction = const $ spawnShell >*> 3
    , defaultTopic = "dashboard"
    , topicActions = M.fromList $
        [ ("fblogs", spawnShell)
        ]
    , maxTopicHistory = 10
    }

workspaceKeys = [ xK_1, xK_2, xK_3, xK_4, xK_5, xK_6, xK_7, xK_8, xK_9, xK_0 ]

spawnShell :: X ()
spawnShell = currentTopicDir myTopicConfig >>= spawnShellIn

spawnShellIn :: Dir -> X ()
spawnShellIn dir = spawn $ "/opt/local/bin/urxvt -cd " ++ dir

goto :: Topic -> X ()
goto = switchTopic myTopicConfig

promptedGoto :: X ()
promptedGoto = workspacePrompt defaultXPConfig goto

promptedShift :: X ()
promptedShift = workspacePrompt defaultXPConfig $ windows . W.shift

myPP :: PP
myPP = defaultPP
    { ppSep = " | "
    , ppTitle = xmobarColor "green" ""
    , ppCurrent = xmobarColor "yellow" ""
    , ppUrgent = xmobarColor "red" "yellow"
    }

mergePPOutputs :: [PP -> X String] -> PP -> X String
mergePPOutputs x pp = fmap (intercalate (ppSep pp)) . sequence . sequence x $ pp

onlyTitle :: PP -> PP
onlyTitle pp = defaultPP { ppCurrent = const ""
                         , ppHidden  = const ""
                         , ppVisible = const ""
                         , ppLayout  = ppLayout pp
                         , ppTitle   = ppTitle pp }

myDynamicLogString :: TopicConfig -> PP -> X String
myDynamicLogString tg pp = mergePPOutputs [TS.pprWindowSet tg, dynamicLogString . onlyTitle] pp

myDynamicLogWithPP :: TopicConfig -> PP -> X ()
myDynamicLogWithPP tg pp = myDynamicLogString tg pp >>= io . ppOutput pp


myKeys =
    [ ((mod1Mask .|. shiftMask, xK_s), spawn "/opt/local/bin/urxvt -pe spdev")
    , ((mod1Mask .|. shiftMask, xK_d), spawn "/opt/local/bin/urxvt -pe 'dev<dev2>'")
    , ((mod1Mask .|. shiftMask, xK_f), spawn "/opt/local/bin/urxvt -pe 'dev<s4hb-pkr-dev1>'")
    , ((mod1Mask              , xK_n), spawnShell)
    , ((mod1Mask              , xK_a), currentTopicAction myTopicConfig)
    , ((mod1Mask              , xK_g), promptedGoto)
    , ((mod1Mask .|. shiftMask, xK_g), promptedShift)
    , ((mod1Mask .|. shiftMask, xK_BackSpace), removeWorkspace)
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
    ++
    [ ((mod1Mask, k), switchNthLastFocused myTopicConfig i)
    | (i, k) <- zip [1..] workspaceKeys]

myManageHook = composeAll
    [ resource =? "Do" --> doIgnore
    ]


main = do
    h <- spawnPipe "/usr/local/bin/xmobar"
    checkTopicConfig myTopics myTopicConfig
    xmonad $ defaultConfig
        { terminal = "/opt/local/bin/urxvt"
        , focusFollowsMouse = False
        , focusedBorderColor = "#2F95ED"
        , borderWidth = 5
        , manageHook = manageDocks <+> myManageHook <+> manageHook defaultConfig
        , layoutHook = avoidStruts  $  layoutHook defaultConfig
        , logHook = myDynamicLogWithPP myTopicConfig $ myPP { ppOutput = hPutStrLn h }
        , workspaces = myTopics
        }
        `additionalKeys` myKeys

