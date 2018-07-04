import XMonad
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.SetWMName

import XMonad.Util.Run        (spawnPipe)
import XMonad.Util.EZConfig   (additionalKeys)
import XMonad.Util.SpawnOnce

import qualified Data.List as L
import qualified Data.Text as T
import qualified XMonad.StackSet as W

import XMonad.Layout.Spacing
import XMonad.Layout.ResizableTile
import XMonad.Layout.WindowArranger
import XMonad.Layout.NoBorders
import XMonad.Layout.Fullscreen
import XMonad.Layout.Gaps

import qualified XMonad.Layout.IndependentScreens as IndependentScreens
import qualified XMonad.Actions.CycleWS as CycleWS

import System.IO

{- Workspaces -}
-- functions for AwesomeWM style multiple monitor setup
getCurrentScreen :: X ScreenId
getCurrentScreen = gets (W.screen . W.current . windowset)

isWSOnScreen :: ScreenId -> WindowSpace -> Bool
isWSOnScreen screen workspace = screen == IndependentScreens.unmarshallS (W.tag workspace)

workSpacesOnCurrentScreen :: CycleWS.WSType
workSpacesOnCurrentScreen = CycleWS.WSIs $ isWSOnScreen <$> getCurrentScreen

moveToWSOnCurrentScreen:: Direction1D -> X ()
moveToWSOnCurrentScreen direction = CycleWS.moveTo direction workSpacesOnCurrentScreen

shiftToWSOnCurrentScreen:: Direction1D -> X ()
shiftToWSOnCurrentScreen direction = CycleWS.shiftTo direction workSpacesOnCurrentScreen


startUpActions :: X ()
startUpActions = do
    spawnOnce "compton --config ~/.xmonad/compton.conf"
    spawnOnce "feh --bg-scale ~/.xmonad/background.jpg"
    spawnOnce "xmobar ~/.xmonad/upperxmobarconf"

    {- setWMName "LG3D" -}

{- Layout -}
defaultLayout = ResizableTall 1 (2/100) (1/2) []

layout = (gaps [(U, 20), (R, 8), (L, 8), (D, 20)] $
          avoidStruts (spacing 2 $ defaultLayout)) ||| Full
------------

main :: IO () 
main = do
    xmobarHandle <- spawnPipe "xmobar ~/.xmonad/lowerxmobarconf"
    numberOfScreens <- IndependentScreens.countScreens
    xmonad $ def
            { manageHook = manageDocks <+> manageHook def
            , layoutHook = windowArrange layout
            , startupHook = startUpActions
            , workspaces = IndependentScreens.withScreens numberOfScreens $ map (:[]) ['1'..'9']
            , terminal = myTerminal
            , modMask =  mod4Mask
            , borderWidth = 3
            , logHook = dynamicLogWithPP xmobarPP
                {
                  ppOutput = hPutStrLn xmobarHandle
                , ppTitle = xmobarColor "#50fa7b" "" . shorten 50
                , ppLayout = (\_ -> "")
                }
        , focusedBorderColor = focdBord
        , normalBorderColor = normBord
        } `additionalKeys` mKeys

{- Keys -}
mKeys = [ 
        -- workspace navigation
          ((modMask, xK_h), moveToWSOnCurrentScreen CycleWS.Prev )
        , ((modMask .|. shiftMask, xK_h), shiftToWSOnCurrentScreen CycleWS.Prev)
        , ((modMask, xK_l), moveToWSOnCurrentScreen CycleWS.Next)
        , ((modMask .|. shiftMask, xK_l), shiftToWSOnCurrentScreen CycleWS.Next)

        -- screen navigation
        , ((modMask, xK_w), CycleWS.prevScreen)
        , ((modMask .|. shiftMask, xK_w), CycleWS.shiftPrevScreen)
        , ((modMask, xK_e), CycleWS.nextScreen)
        , ((modMask .|. shiftMask, xK_e), CycleWS.shiftNextScreen)

        -- application startup
        , ((modMask , xK_Return), do
                windowtitle <- getCurrentWindowTitle
                let (app, path) = span (/= '/') windowtitle
                    app' = T.unpack .  T.strip . T.pack $ app
                    path' = T.unpack .  T.strip . T.pack $ path
                    newterminalspawndir = getNextTerminalPath (app', path')
                spawn $ myTerminal ++ " -d " ++ newterminalspawndir) -- spawn terminal at current directoty

        , ((modMask .|. shiftMask, xK_Return), spawn $ myTerminal ++ "-d ~")  -- spawn terminal at home directotry
        , ((modMask, xK_b), spawn myBrowser) -- open browser
        , ((modMask .|. shiftMask, xK_b), spawn $ myBrowser ++ "--private-window")  -- open private instance of browser
        , ((modMask, xK_n), spawn $ myTerminal ++ " -x " ++ "'" ++ myMusicPlayer ++ "'")  -- open ncmpcpp
        , ((modMask, xK_r), spawn $ "rofi -show run")  

        -- volume control
        , ((0, xK_F11 ), spawn  "amixer set 'Master' 2%-")
        , ((0, xK_F12 ), spawn "amixer set 'Master' 2%+")

    ] where modMask = mod4Mask  -- prefer super

-- returns the terminal path if the title is one of the following applications
getNextTerminalPath :: (String, String) -> String
getNextTerminalPath ("n", path) = path
getNextTerminalPath ("v", path) = path
getNextTerminalPath ("ghci", path) = path
getNextTerminalPath ("vim", path) = path
getNextTerminalPath ("fish", path) = path
getNextTerminalPath ("clang", path) = path
getNextTerminalPath ("stack", path) = path
getNextTerminalPath (_, _) = "~"

-- getting the current window title 
getCurrentWindowTitle :: X String
getCurrentWindowTitle = dynamicLogString $ (def PP) { ppCurrent = (\_ -> "")
                                                    , ppVisible = (\_ -> "")
                                                    , ppHidden = (\_ -> "")
                                                    , ppHiddenNoWindows = (\_ -> "")
                                                    , ppUrgent = (\_ -> "")
                                                    , ppSep = ""
                                                    , ppWsSep = ""
                                                    , ppLayout = (\_ -> "") }

{- Programs -}
myTerminal = "sakura "
myBrowser = "firefox "
myMusicPlayer = "ncmpcpp -b ~/.config/ncmpcpp/config"

{- Colors -}
focdBord = "#f8f8f2"
normBord = "#282a36"
