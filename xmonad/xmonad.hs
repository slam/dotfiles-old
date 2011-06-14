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
import Control.Concurrent

import XMonad.Actions.TopicSpace as TS
import XMonad.Prompt
import XMonad.Prompt.Shell
import XMonad.Prompt.Workspace
import qualified Data.Map as M
import Data.List

import XMonad.Layout.OneBig
import XMonad.Layout.PerWorkspace (onWorkspace, onWorkspaces)

--
-- Stole lots of ideas on TopicSpace configuation from:
--
-- http://github.com/aniederl/config-xmonad/blob/master/xmonad.hs
--
myTopics :: [Topic]
myTopics =
    [ "prodlogs"
    , "stage1"
    , "stage1-2"
    , "dev1"
    , "devlogs"
    , "dev2"
    , "wireshark"
    , "dev2logs"
    , "xmonad"
    , "dashboard"
    ]

myTopicConfig :: TopicConfig
myTopicConfig = TopicConfig
    { topicDirs = M.fromList $
        [ ("dashboard", "~/")
        ]
    , defaultTopicAction = const $ spawnShell >*> 3
    , defaultTopic = "dashboard"
    , topicActions = M.fromList $
        [ ("prodlogs", spawnScreen "stage1:sahn.16:#CBD4BC"
                  >> spawnScreen "stage1:sahn.15:#CBD4BC"
                  >> spawnScreen "stage1:sahn.14:#CBD4BC"
                  >> spawnScreen "stage1:sahn.13:#CBD4BC"
                  >> spawnScreen "stage1:sahn.12:#CBD4BC"
                  >> spawnScreen "stage1:sahn.11:#CBD4BC"
                  )
        , ("stage1", spawnScreen "stage1:sahn.25:#E2E3D1"
                  >> spawnScreen "stage1:sahn.24:#E2E3D1"
                  >> spawnScreen "stage1:sahn.23:#E2E3D1"
                  >> spawnScreen "stage1:sahn.22:#E2E3D1"
                  >> spawnScreen "stage1:sahn.21:#E2E3D1"
                  )
        , ("stage1-2", spawnScreen "stage1:sahn.36:#EEF5C1"
                  >> spawnScreen "stage1:sahn.35:#EEF5C1"
                  >> spawnScreen "stage1:sahn.34:#EEF5C1"
                  >> spawnScreen "stage1:sahn.33:#EEF5C1"
                  >> spawnScreen "stage1:sahn.32:#EEF5C1"
                  >> spawnScreen "stage1:sahn.31:#EEF5C1"
                  )
        , ("dev1",   spawnScreen "dev1:sahn.15:#E0F0E2"
                  >> spawnScreen "dev1:sahn.14:#E0F0E2"
                  >> spawnScreen "dev1:sahn.13:#E0F0E2"
                  >> spawnScreen "dev1:sahn.12:#E0F0E2"
                  >> spawnScreen "dev1:sahn.11:#E0F0E2"
                  >> spawnScreen "dev1::#DDE8F0"
                  )
        , ("dev2",   spawnScreen "dev2:sahn.15:#DDE8F0"
                  >> spawnScreen "dev2:sahn.14:#DDE8F0"
                  >> spawnScreen "dev2:sahn.13:#DDE8F0"
                  >> spawnScreen "dev2:sahn.12:#DDE8F0"
                  >> spawnScreen "dev2:sahn.11:#DDE8F0"
                  >> spawnScreen "dev2::#E0F0E2"
                  )

        , ("devlogs",
                     spawnScreen "dev1:access_log:#E0F0E2"
                  >> spawnScreen "dev1:error_log:#E0F0E2"
                  )
        , ("dev2logs",
                     spawnScreen "dev2:access_log:#E0F0E2"
                  >> spawnScreen "dev2:error_log:#E0F0E2"
                  )
        , ("xmonad", spawnScreenIn "localhost:dot_urxvt" "~/.urxvt"
                  >> spawnScreenIn "localhost:dot_xmonad" "~/.xmonad"
                  >> spawnScreenIn "localhost:dot_xmcontrib" "/usr/local/src/xmonad-contrib-0.9.1/XMonad/"
                  )
        ]
    , maxTopicHistory = 10
    }

workspaceKeys = [ xK_1, xK_2, xK_3, xK_4, xK_5, xK_6, xK_7, xK_8, xK_9, xK_0 ]

spawnShell :: X ()
spawnShell = currentTopicDir myTopicConfig >>= spawnShellIn

spawnShellIn :: Dir -> X ()
spawnShellIn dir = spawn $ "/opt/local/bin/urxvt -cd " ++ dir

urxvtWithExt :: String -> String -> String -> String
urxvtWithExt extension args [] = "/opt/local/bin/urxvt -pe '" ++ extension ++ "<" ++ args ++ ">'"
urxvtWithExt extension args dir = "/opt/local/bin/urxvt -cd " ++ dir ++ " -pe '" ++ extension ++ "<" ++ args ++ ">'"

spawnScreen :: String -> X ()
spawnScreen args = currentTopicDir myTopicConfig >>= spawnScreenIn args

spawnScreenIn :: String -> Dir -> X ()
spawnScreenIn args dir = spawnThenSleep $ (urxvtWithExt "screen" args dir)

spawnIn :: String -> Dir -> X ()
spawnIn program dir = spawnThenSleep $ program

-- To make sure windows spawned in quick succession follow some deterministic
-- order.
spawnThenSleep :: MonadIO m => String -> m ()
spawnThenSleep x = spawn x >> catchIO (threadDelay 500000)

goto :: Topic -> X ()
goto = switchTopic myTopicConfig

promptedGoto :: X ()
promptedGoto = workspacePrompt defaultXPConfig goto

promptedShift :: X ()
promptedShift = workspacePrompt defaultXPConfig $ windows . W.shift

myKeys =
    [ ((mod1Mask              , xK_n), spawnShell)
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
    -- mod-[1..9] %! Switch to workspace N
    -- mod-shift-[1..9] %! Move client to workspace N
    [((m .|. mod1Mask, k), windows $ f i)                     
        | (i, k) <- zip myTopics workspaceKeys
        , (f, m) <- [(W.greedyView, 0), (W.shift, shiftMask)]]
--    ++
--    [ ((mod1Mask, k), switchNthLastFocused myTopicConfig i)
--    | (i, k) <- zip [1..] workspaceKeys]

myManageHook = composeAll
    [ resource =? "Do" --> doIgnore
    ]

myXmobarPP :: PP
myXmobarPP = defaultPP { ppCurrent = xmobarColor "yellow" "" . wrap "[" "]"
    , ppTitle   = xmobarColor "green"  ""
    , ppVisible = wrap "(" ")"
    , ppUrgent = xmobarColor "red" "yellow"
}

layoutCode = OneBig (3/4) (3/4)

main = do
    h <- spawnPipe "/usr/local/bin/xmobar"
    checkTopicConfig myTopics myTopicConfig
    xmonad $ defaultConfig
        { terminal = "/opt/local/bin/urxvt"
        , focusFollowsMouse = False
        , focusedBorderColor = "#2F95ED"
        , borderWidth = 5
        , manageHook = manageDocks <+> myManageHook <+> manageHook defaultConfig
        , layoutHook = avoidStruts
                     $ onWorkspaces [ "dev1"
                                    , "dev2"
                                    ] layoutCode
                     $ layoutHook defaultConfig
        , logHook = dynamicLogWithPP $ myXmobarPP { ppOutput = hPutStrLn h }
        , workspaces = myTopics
        }
        `additionalKeys` myKeys

