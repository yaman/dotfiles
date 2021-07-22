import Data.Map (fromList)
import Data.Monoid (mappend)
import XMonad
import XMonad.Actions.SpawnOn
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.SetWMName
import XMonad.Layout.NoBorders (smartBorders)
import XMonad.Layout.Spacing (smartSpacing)
import XMonad.Util.EZConfig (additionalKeysP)
import XMonad.Actions.CycleWS
import Data.List (sortBy)
import Data.Function (on)
import Control.Monad (forM_, join, liftM2)
import XMonad.Util.Run (safeSpawn)
import XMonad.Util.NamedWindows (getName)
import qualified XMonad.StackSet as W


-- The main function.
-- main = xmonad =<< statusBar myBar myPP toggleStrutsKey myConfig
main = do 
	xmonad =<< statusBar myBar myPP toggleStrutsKey myConfig
	forM_ [".xmonad-workspace-log", ".xmonad-title-log"] $ \file -> do
    		safeSpawn "mkfifo" ["/tmp/" ++ file]

-- -- Command to launch the bar.
myBar = "echo x"

--
-- -- Custom PP, configure it as you like. It determines what is being written to the bar.
-- myPP = xmobarPP {ppCurrent = xmobarColor "#829942" "" . wrap "<" ">"}
myPP = xmobarPP {ppCurrent = xmobarColor "#FFFFFF" "" . wrap "<" ">"}

--
-- -- Key binding to toggle the gap for the bar.
toggleStrutsKey XConfig {XMonad.modMask = modMask} = (modMask, xK_b)

shortcuts =
  [ ("C-F12", spawn "sleep 0.2; scrot -s")
  , ("<F12>", spawn "scrot")
  , ("<F11>", spawn "mate-screensaver-command -l")
  ]

myManageHook = composeAll
   [ className =? "termite" --> viewShift "terminal"
   , className =? "firefox" --> viewShift "www"
   , className =? "discord"  --> viewShift "dc"
   , className =? "telegram-desktop"  --> viewShift "comms"
   , manageDocks
   ]
   where viewShift = doF . liftM2 (.) W.greedyView W.shift

--
-- -- Main configuration, override the defaults to your liking.
myConfig = ewmh defaultConfig
    { terminal = myTerminal
    , modMask = myModMask
    , borderWidth = myBorderWidth
    , focusedBorderColor = myFocusedBorderColor
    , normalBorderColor = myNormalBorderColor
    , workspaces = myWorkspaces
    , layoutHook = smartSpacing 2 $ smartBorders $ layoutHook defaultConfig
    , logHook         = eventLogHook
    , manageHook =
        myManageHook <+> manageHook defaultConfig 
    , handleEventHook = handleEventHook defaultConfig <+> fullscreenEventHook
     -- , startupHook = setWMName "LG3D"
    , startupHook = myStartupHook <+> ewmhDesktopsStartup >> setWMName "LG3D"
    } `additionalKeysP`
  shortcuts

-- perApplicationHook =
--   composeAll [className =? "Wine" --> doFloat, className =? "Steam" --> doFloat]
--   where
--     role = stringProperty "WM_WINDOW_ROLE"

myStartupHook :: X ()
myStartupHook = do
--  spawnOn "" "feh --randomize --bg-fill ~/Pictures/walpapers/*"
--  spawnOn "" "xautolock -time 5 -locker xscreensaver-command -lock"
  spawnOn "" "feh --randomize --bg-fill ~/.config/variety/Downloaded/**/*"
  spawnOn "" "variety --resume"
  spawnOn "" "xbindkeys -p"
  spawnOn "" "fusuma"
  spawnOn "" "mate-screensaver"
  spawnOn "" "xmodmap ~/.Xmodmap"
  spawnOn "" "exec ~/.bin/launch_poly.sh"

myTerminal = "termite"

myModMask = mod4Mask -- Win key or Super_L

-- width of border around windows
myBorderWidth = 1

-- color of focused border
myFocusedBorderColor = "#ffffff"

-- color of inactive border
myNormalBorderColor = "#000000"

myWorkspaces = ["www", "dc", "terminal", "comms", "utilities"]

eventLogHook = do
  winset <- gets windowset
  title <- maybe (return "") (fmap show . getName) . W.peek $ winset
  let currWs = W.currentTag winset
  let wss = map W.tag $ W.workspaces winset
  let wsStr = join $ map (fmt currWs) $ sort' wss

  io $ appendFile "/tmp/.xmonad-title-log" (title ++ "\n")
  io $ appendFile "/tmp/.xmonad-workspace-log" (wsStr ++ "\n")

  where fmt currWs ws
          | currWs == ws = "[" ++ ws ++ "]"
          | otherwise    = " " ++ ws ++ " "
        sort' = sortBy (compare `on` (!! 0))
