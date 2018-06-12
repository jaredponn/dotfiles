import XMonad
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.SetWMName

import XMonad.Util.Run        (spawnPipe)
import XMonad.Util.EZConfig   (additionalKeys)
import XMonad.Util.SpawnOnce

import Data.List
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
                , ppTitle = xmobarColor "green" "" . shorten 50
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
        , ((modMask , xK_Return), spawn $ myTerminal ++ "-d \"`xcwd`\"") -- spawn terminal at current directoty
        , ((modMask .|. shiftMask, xK_Return), spawn $ myTerminal ++ "-d ~")  -- spawn terminal at home directotry
        , ((modMask, xK_b), spawn myBrowser) -- open browser
        , ((modMask .|. shiftMask, xK_b), spawn $ myBrowser ++ "--private-window")  -- open private instance of browser

    ] where modMask = mod4Mask  -- prefer super

{- Programs -}
myTerminal = "sakura "
myBrowser = "firefox "

{- Colors -}
focdBord = "#f8f8f2"
normBord = "#282a36"
