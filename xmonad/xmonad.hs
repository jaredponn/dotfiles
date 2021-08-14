-- XMonad
import XMonad as X
import qualified XMonad.StackSet as W

-- XMonad.Contrib
import qualified XMonad.Hooks.ManageDocks as XMonad.Contrib
import qualified XMonad.Layout.Gaps as XMonad.Contrib
import qualified XMonad.Hooks.DynamicLog as XMonad.Contrib
import qualified XMonad.Util.Run as XMonad.Contrib
import qualified XMonad.Util.Loggers as XMonad.Contrib

-- External libraries
import qualified Graphics.X11.Xlib.Extras
import qualified Graphics.X11.ExtraTypes.XF86
import qualified System.Process

-- Common data structures
import Data.Map (Map)
import qualified Data.Map as M 

-- Haskell base
import Data.Function
import Data.Maybe
import Data.List
import Data.Tuple
import Data.Char
import Control.Arrow ((***))
import Control.Monad
import System.Exit
import System.IO

import qualified System.Exit 

-- Note:
-- The module XMonad.Config is contains the default XMonad bindings for reference...;
-- It can be found here:
-- https://hackage.haskell.org/package/xmonad-0.15/docs/src/XMonad.Config.html

main :: IO ()
main = do
    xmobarpipe <- XMonad.Contrib.spawnPipe "xmobar"
    xmonad $ XMonad.Contrib.docks $ def
        {
            modMask = mod4Mask
            , startupHook = configStartupHooks
            , keys = configKeys

            , logHook = XMonad.Contrib.dynamicLogWithPP $ def 
                { XMonad.Contrib.ppOutput = hPutStrLn xmobarpipe 
                , XMonad.Contrib.ppLayout = const ""
                }

            , layoutHook = configLayout
            
            , terminal = "alacritty"
            , focusedBorderColor = "#f8f8f2"
            , normalBorderColor = "#282a36"
            , borderWidth = 3
            
            , focusFollowsMouse = False
            , clickJustFocuses = False

            , workspaces = map show ([1..9] ++ [0]) ++ ["-", "="]

            , manageHook = configManageHook
        }

configManageHook :: ManageHook
configManageHook = composeAll 
    [  className =? "zoom" --> doFloat
    ,  className =? "wechat.exe" --> doFloat
    ,  className =? "wechat" --> doFloat
    ]

configStartupHooks :: X ()
configStartupHooks = do 
    spawn "~/.screenlayout/home.sh"

-- configLayout :: Layout Window
-- configLayout = tiled ||| Mirror tiled ||| Full
configLayout = XMonad.Contrib.avoidStruts 
     $ tiled
     -- $ (XMonad.Contrib.gaps [(XMonad.Contrib.U,5)] tiled )
     -- $ (XMonad.Contrib.gaps [(XMonad.Contrib.U,5)] tiled )
     ||| Full
  where
     -- default tiling algorithm partitions the screen into two panes
     tiled   = Tall nmaster delta ratio

     -- The default number of windows in the master pane
     nmaster = 1

     -- Default proportion of screen occupied by master pane
     ratio   = 1/2

     -- Percent of screen to increment by when resizing panes
     delta   = 3/100

{- To find key codes, search in
@
/usr/include/X11/keysymdef.h
@
and note that XK* becomes xK*.
To find the actual key code, type 
@
xev
@
in the terminal, and press the key you want.
Something like 
@
KeyPress event, serial 32, synthetic NO, window 0xda00001,
    root 0x93b, subw 0x0, time 3199770520, (314,465), root:(3081,468),
    state 0x0, keycode 20 (keysym 0x2d, minus), same_screen YES,
    XLookupString gives 1 bytes: (2d) "-"
    XmbLookupString gives 1 bytes: (2d) "-"
    XFilterEvent returns: False
@
will show up, and we're interested in
@
state 0x0, keycode 20 (keysym 0x2d, minus), same_screen YES,
@
which tells us that we want to use the key code 0x002d, which corresponds
to
@
XK_minus
@
in
@
/usr/include/X11/keysymdef.h
@

-}
configKeys :: XConfig Layout -> Map (KeyMask, KeySym) (X ())
configKeys conf@(XConfig {X.modMask = modMask}) = (`M.union` X.keys X.def conf) $ M.fromList $
-- configKeys conf@(XConfig {X.modMask = modMask}) = M.fromList $
    -- launching and killing programs
    [ ( (modMask .|. shiftMask, xK_Return), spawn $ X.terminal conf) -- %! Launch terminal at root directory
    , ( (modMask              , xK_Return)
        , X.withDisplay $ \display -> 
            X.withWindowSet $ \wset -> flip (maybe (spawn $ X.terminal conf)) (W.peek wset) 
                $ \windowxid -> do
                    netwmpidatom <- X.getAtom "_NET_WM_PID"
                    windowprop32query <- X.io $ Graphics.X11.Xlib.Extras.getWindowProperty32 
                        display 
                        netwmpidatom 
                        windowxid
                    case windowprop32query of 
                        Just [querypid] -> do
                            let pid = show querypid
                            -- spawn ("echo " ++ show windowxid ++ "aya" ++ show pid ++ concat ["/proc/" ++ show pid ++ "/comm"]++ " | xmessage -file -")
                            (exeexitcode, exestdout, exestderr) <- X.io
                                $ readProcessWithExitCode 
                                    "cat"
                                    ["/proc/" ++ pid ++ "/comm"]
                                    ""
                            (childexitcode, childstdout, childstderr) <- X.io
                                $ readProcessWithExitCode 
                                    "pgrep"
                                    ["-P", pid]
                                    ""
                            let childpid = trim childstdout
                            (cwdexitcode, cwdstdout, cwdstderr) <- X.io
                                $ readProcessWithExitCode 
                                    "readlink"
                                    ["/proc/" ++ childpid ++ "/cwd"]
                                    ""
                            let childcwd = escapeDir $ trim cwdstdout
                            -- spawn ("echo " ++ "exe stdout:" ++ show exestdout ++ " childstdout " ++ show childstdout ++ " final cwd: " ++ show childcwd ++ " | xmessage -file -")
                            if exeexitcode == ExitSuccess
                                && trim exestdout == (X.terminal conf)
                                && childexitcode == ExitSuccess
                                && cwdexitcode == ExitSuccess
                                    then spawn $ (X.terminal conf ++ " --working-directory " ++ childcwd )
                                    else spawn $ X.terminal conf
                        Nothing -> spawn $ X.terminal conf
        -- Here are the steps...
        -- 1. Get the pid of the focused process <PID>
        -- 2. Check if the focused process is the terminal
        -- 3. If it is, then continue, otherwise give up
        -- 4. Get the children of the terminal process <PID> with `pgrep -P <PID>`
        -- 5. If there is exactly one child, continue (hopefully the bash child), otherwise give up
        --  (Not too sure about the guarantees of this step!)
        -- 6. With the PID of the only child, <PIDC>, run `readlink /proc/<PIDC>/cwd` to get the working directory of the child
        -- 7. If anything ``gave up", just spawn a terminal at the home directory!
        
        ) -- %! Launch terminal at previous terminal directory if it exists.
    , ( (modMask              , xK_b     ), spawn $ "chromium") -- %! Launch browser
    , ( (modMask .|. shiftMask, xK_b     ), spawn $ "chromium --incognito") -- %! Launch browser
    , ( (modMask              , xK_x     ), spawn $ "xournalpp")                -- %! Launch xournalpp
    , ( (modMask              , xK_d     )
        , spawn $ "source ~/.xmonad/xmonadDmenu.sh"
      ) -- %! runs dmenu from the script
    , ( (noModMask            , xK_Print ), spawn $ "flameshot gui")           
        -- %! Open flameshot for screenshots

    -- , ((modMask,               xK_p     ), spawn "dmenu_run") -- %! Launch dmenu
    -- , ((modMask .|. shiftMask, xK_p     ), spawn "gmrun") -- %! Launch gmrun
    , ( (modMask .|. shiftMask, xK_c     ), kill) -- %! Close the focused window

    , ( (modMask,               xK_space ), sendMessage NextLayout) -- %! Rotate through the available layout algorithms
    , ( (modMask .|. shiftMask, xK_space ), setLayout $ X.layoutHook conf) -- %!  Reset the layouts on the current workspace to default

    , ( (modMask,               xK_n     ), refresh) -- %! Resize viewed windows to the correct size

    -- move focus up or down the window stack
    , ( (modMask,               xK_Tab   ), windows W.focusDown) -- %! Move focus to the next window
    , ( (modMask .|. shiftMask, xK_Tab   ), windows W.focusUp  ) -- %! Move focus to the previous window
    , ( (modMask,               xK_j     ), windows W.focusDown) -- %! Move focus to the next window
    , ( (modMask,               xK_k     ), windows W.focusUp  ) -- %! Move focus to the previous window
    , ( (modMask,               xK_m     ), windows W.focusMaster  ) -- %! Move focus to the master window

    -- Modifying sound 
    -- https://hackage.haskell.org/package/X11-1.9.2/docs/Graphics-X11-ExtraTypes-XF86.html#v:xF86XK_AudioLowerVolume
    , ( (noModMask, Graphics.X11.ExtraTypes.XF86.xF86XK_AudioLowerVolume), spawn "pulsemixer --change-volume -5") 
        -- %! Decrease volume
    , ( (noModMask, Graphics.X11.ExtraTypes.XF86.xF86XK_AudioRaiseVolume), spawn "pulsemixer --change-volume +5") 
        -- %! Increase volume

    -- modifying the window order
    , ( (modMask .|. shiftMask, xK_m), windows W.swapMaster) -- %! Swap the focused window and the master window
    , ( (modMask .|. shiftMask, xK_j), windows W.swapDown  ) -- %! Swap the focused window with the next window
    , ( (modMask .|. shiftMask, xK_k), windows W.swapUp    ) -- %! Swap the focused window with the previous window

    --  resizing the master/slave ratio
    , ( (modMask,               xK_h     ), sendMessage Shrink) -- %! Shrink the master area
    , ( (modMask,               xK_l     ), sendMessage Expand) -- %! Expand the master area

    -- floating layer support
    , ( (modMask,               xK_t     ), withFocused $ windows . W.sink) -- %! Push window back into tiling

    -- increase or decrease number of windows in the master area
    , ( (modMask              , xK_comma ), sendMessage (IncMasterN 1)) -- %! Increment the number of windows in the master area
    , ( (modMask              , xK_period), sendMessage (IncMasterN (-1))) -- %! Deincrement the number of windows in the master area


    -- quit, or restart
    , ( (modMask .|. shiftMask, xK_q     ), io (System.Exit.exitWith System.Exit.ExitSuccess)) -- %! Quit xmonad
    , ( (modMask              , xK_q     ), spawn "if type xmonad; then xmonad --recompile && xmonad --restart; else xmessage xmonad not in \\$PATH: \"$PATH\"; fi") -- %! Restart xmonad

    , ( (modMask .|. shiftMask, xK_slash ), helpCommand) -- %! Run xmessage with a summary of the default keybindings (useful for beginners)
    -- repeat the binding for non-American layout keyboards
    , ( (modMask              , xK_question), helpCommand) -- %! Run xmessage with a summary of the default keybindings (useful for beginners)
    ]
    ++
    -- mod-[1..9] %! Switch to workspace N
    -- mod-shift-[1..9] %! Move client to workspace N
    [ ( (m .|. modMask, k), X.windows $ f i)
        | (i, k) <- zip (X.workspaces conf) ([xK_1 .. xK_9] ++ [xK_0, xK_minus, xK_equal])
        -- , (f, m) <- [(W.greedyView, 0), (W.shift, shiftMask)]
        , (f, m) <- 
            [ (W.view, noModMask)
            , (W.greedyView, controlMask)
            , (W.shift, shiftMask)
            ]
    ]
    ++
    -- mod-{w,e,r} %! Switch to physical/Xinerama screens 1, 2, or 3
    -- mod-shift-{w,e,r} %! Move client to screen 1, 2, or 3
    -- OLD DEFAULT XMONAD:
    -- [ ((m .|. modMask, key), X.screenWorkspace sc 
    --     >>= flip whenJust (windows . f))
    --     | (key, sc) <- zip [xK_w, xK_e, xK_r] [0..]
    --     , (f, m) <- [(W.view, noModMask), (W.shift, shiftMask)]
    -- ]
    -- mod-{w,e} %! Cycle to physical/Xinerama screens
    -- mod-shift-{w,e} %! Move client to screen by cycling
    [ ( (m .|. modMask, xK_w)
        , X.withWindowSet $ \wset -> do
            let curscreenid = W.screen $ W.current wset  
                visiblescreensids = fmap W.screen $ W.visible wset  
                prevscreen 
                    | not $ null visiblescreensids = 
                        head
                        $ tail
                        $ cycle
                        $ (curscreenid:)
                        -- we are interested in the screens which come before the
                        -- current screen..
                        $ uncurry (<>) 
                        $ (reverse *** reverse)
                        $ partition (<=curscreenid) 
                        $ sort visiblescreensids
                    -- Here's what it does...
                    -- 1) Given screens [0,1,2,3] with focus on 1
                    -- 2) Partition to ([0], [2,3])
                    -- 3) Sort both partitions in reverse order ([0], [3,2])
                    -- 4) Concat together [0,3,2]
                    -- 5) Prepend the focused screen (in case there are no other screens) [1,0,3,2]
                    -- 6) Cycle the list [1,0,3,2, .... ]
                    -- 7) Ignore the first element of the list [0,3,2, .... ] 
                    --  (which always corresponds to the current screen) 
                    -- 8) Then, take the head as the previous screen i.e.,  0
                    | otherwise = curscreenid
            prevscreenworkspace <- X.screenWorkspace prevscreen
            whenJust prevscreenworkspace (windows . f)
        )
        | (f, m) <- [(W.view, noModMask), (W.shift, shiftMask)]
    ]
    ++
    -- mostly duplicated code for xK_e
    [ ( (m .|. modMask, xK_e)
        , X.withWindowSet $ \wset -> do
            let curscreenid = W.screen $ W.current wset  
                visiblescreensids = fmap W.screen $ W.visible wset  
                nextscreen 
                    | not $ null visiblescreensids = 
                        head
                        $ tail
                        $ cycle
                        -- can't remember exactly why this needs to be commented out, but it appears to work wihtout it.
                        -- $ (curscreenid:)
                        $ uncurry (flip (<>))
                        $ (reverse *** reverse )
                        $ partition (>=curscreenid) 
                        $ sort visiblescreensids
                    | otherwise =  curscreenid
            nextscreenworkspace <- X.screenWorkspace nextscreen
            whenJust nextscreenworkspace (windows . f)
        )
        | (f, m) <- [(W.view, noModMask), (W.shift, shiftMask)]
    ]
    -- Resizing a floating window:
    -- mod + left dragging is moving a floating window..
    -- mod + right dragging is moving a resizing a floating window..
  where
    helpCommand :: X ()
    helpCommand = spawn ("echo " ++ show help ++ " | xmessage -file -")

    help = ""

-- This modified readProcessWithExitCode seems to work with XMonad in place
-- of the default System.Process.readProcessWithExitCode which does not work!
readProcessWithExitCode :: 
    FilePath ->                     -- filename of executable
    [String] ->                     -- arguments
    String ->                       -- stdin
    IO (ExitCode, String, String)   -- (exit code, stdout, stderr)
readProcessWithExitCode cmd args input = io $ do
        (pin, pout, perr, phand) <- System.Process.runInteractiveProcess cmd args Nothing Nothing
        System.IO.hPutStr pin input
        System.IO.hClose pin
        output <- System.IO.hGetContents pout
        erroutput <- System.IO.hGetContents perr

        Control.Monad.when (output == output && erroutput == erroutput) $ return ()
        merrcode <- System.Process.getProcessExitCode phand
        let errcode = case merrcode of 
                        (Just x) -> x
                        Nothing -> ExitSuccess

        System.IO.hClose pout
        System.IO.hClose perr
        -- no need to waitForProcess, we ignore SIGCHLD
        return (errcode, output, erroutput)

-- trims the whitespace from a string according to Data.isSpace
trim :: 
    String ->
    String 
trim = let f = reverse . dropWhile isSpace in f . f

-- escapes the directory
escapeDir ::
    FilePath ->
    FilePath 
escapeDir = foldr f []
  where
    f ' ' = (['\\', ' ']++)
    f '\"' = (['\\', '\"']++)
    f '\'' = (['\\', '\'']++)
    f n = (n:)
