{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

import XMonad
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.SetWMName

import XMonad.Util.Run              (spawnPipe, runProcessWithInput)
import XMonad.Util.EZConfig         (additionalKeys)
import XMonad.Util.SpawnOnce
import XMonad.Util.WindowProperties (getProp32s)

import Data.Maybe
import qualified Data.List as L
import qualified Data.Text as T
import qualified Data.String
import qualified Data.Either

import qualified XMonad.StackSet as W

import XMonad.Core
import XMonad.Layout.Spacing
import XMonad.Layout.ResizableTile
import XMonad.Layout.WindowArranger
import XMonad.Layout.NoBorders
import XMonad.Layout.Fullscreen
import XMonad.Layout.Gaps

import qualified XMonad.Hooks.EwmhDesktops 
import qualified XMonad.Layout.IndependentScreens as IndependentScreens
import qualified XMonad.Actions.CycleWS as CycleWS

import qualified Foreign.C.Types
import qualified System.Directory

import qualified System.IO
import qualified System.Process

import qualified GHC.IO.Exception
import qualified System.IO
import qualified Control.Monad

import qualified Codec.Binary.UTF8.String
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
    --spawnOnce "compton --config ~/.xmonad/compton.conf"
    --spawnOnce "feh --bg-scale ~/.xmonad/background.jpg"
    spawnOnce "xmobar ~/.xmonad/xmobarconf/upperxmobarconf"

    {- setWMName "LG3D" -}

{- Layout -}
layout = gaps [(U, 20), (R, 8), (L, 8), (D, 20)] $ avoidStruts (spacing 2 $ ResizableTall 1 (2/100) (1/2) []) 
                ||| Full

{- Programs -}
myTerminal = "ume"
myBrowser = "firefox"
myMusicPlayer = "ncmpcpp -b ~/.config/ncmpcpp/config"

{- Colors -}
focdBord = "#f8f8f2"
normBord = "#282a36"

{- Keys -}
mKeys :: [((KeyMask, KeySym), X ())]
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
        , ((modMask , xK_Return), openTerminalInFocusedDirectory)

        , ((modMask .|. shiftMask, xK_Return), spawn $ "exec " ++ myTerminal ++ " -d ~")  -- spawn terminal at home directotry
        , ((modMask, xK_b), spawn myBrowser) -- open browser
        , ((modMask .|. shiftMask, xK_b), spawn $ myBrowser ++ " --private-window")  -- open private instance of browser
        , ((modMask, xK_n), spawn $ "exec " ++ myTerminal ++ " -x " ++ "'" ++ myMusicPlayer ++ "'")  -- open ncmpcpp
        , ((modMask, xK_r), spawn "rofi -show run")  

        -- volume control
        , ((0, xK_F11 ), spawn  "amixer set 'Master' 2%-")
        , ((0, xK_F12 ), spawn "amixer set 'Master' 2%+")

        -- resizing windows
        , ((modMask .|. controlMask , xK_h), sendMessage Shrink)  
        , ((modMask .|. controlMask , xK_l), sendMessage Expand)  

    ] where modMask = mod4Mask  -- prefer super

------------

main :: IO () 
main = do
    xmobarHandle <- spawnPipe "xmobar ~/.xmonad/xmobarconf/lowerxmobarconf"
    numberOfScreens <- IndependentScreens.countScreens
    xmonad $ XMonad.Hooks.EwmhDesktops.ewmh def
            { manageHook = manageDocks <+> manageHook def
            , layoutHook = windowArrange layout
            , startupHook = startUpActions
            , workspaces = IndependentScreens.withScreens numberOfScreens $ map (:[]) ['1'..'9']
            , terminal = myTerminal
            , modMask =  mod4Mask
            , borderWidth = 3
            , logHook = dynamicLogWithPP xmobarPP
                {
                  ppOutput = System.IO.hPutStrLn xmobarHandle
                , ppTitle = xmobarColor "#50fa7b" "" . shorten 50
                , ppLayout = const ""
                }
        , focusedBorderColor = focdBord
        , normalBorderColor = normBord
        , focusFollowsMouse = False
        , clickJustFocuses = False
        {- , handleEventHook = handleEventHook def <+> XMonad.Hooks.EwmhDesktops.fullscreenEventHook -}
        } `additionalKeys` mKeys



-- escapes the characters so that fish understands what directory we are using
escapeTerminalPath :: String -> String
escapeTerminalPath (x:xs) 
        | escapeCharacter x = '\\' : x  : escapeTerminalPath xs
        | otherwise = x : escapeTerminalPath xs
escapeTerminalPath [] = []

-- provudes the chaacters to escape
escapeCharacter :: Char -> Bool
escapeCharacter x 
        | x == ' ' = True
        | otherwise = False

debugPrint :: String -> X ()
debugPrint str = spawn $ "xmessage \"" ++ str ++ "\""


openTerminalInFocusedDirectory :: X ()
openTerminalInFocusedDirectory = getFocusedPID          >>= 
                                (\case
                                        Just x -> do 
                                                        --debugPrint (show x) 
                                                        val <- (io (runEitherT (queryProcessPathFromPidIfTerminal (Pid x) myTerminal))) 
                                                        case val of
                                                                (Right dir) -> spawn $ "exec " ++ myTerminal ++  " -d " ++ escapeTerminalPath dir
                                                                (Left (Err (0, _) )) -> spawn $ "exec " ++ myTerminal 
                                                                (Left errmsg) -> debugPrint $ show errmsg
                                        Nothing -> spawn $ "exec " ++ myTerminal 
                                )

getFocusedPID :: X (Maybe Int)
getFocusedPID = gets windowset >>= f
        where
                f ::  WindowSet -> X (Maybe Int)
                f ws = case W.peek ws of
                        Just xid -> getProp32s "_NET_WM_PID" xid >>= g
                        Nothing -> return Nothing

                g :: Maybe [Foreign.C.Types.CLong] -> X (Maybe Int)
                g (Just (x:[])) = return $ Just $ (fromIntegral :: Foreign.C.Types.CLong -> Int) x
                g _ = return Nothing

-- | Apply an 'X' operation to the currently focused window, if there is one.
withFocused :: (Window -> X ()) -> X ()
withFocused f = withWindowSet $ \w -> whenJust (W.peek w) f

strip :: String -> String
strip = T.unpack . T.strip . T.pack

{- Command line wrappers -}
newtype Pid = Pid Int deriving Eq

instance Show Pid where
        show (Pid x) = show x

queryProcessPathFromPidIfTerminal :: Pid
                                  -> String -- Terminal (just name of executable)
                                  -> EitherT Err IO StdOut
queryProcessPathFromPidIfTerminal pid term 
                                = do  
                                     p <- liftEitherT $ isPidProcessExecProcess pid term
                                     liftEitherT $ trace $ "ZENMELE" ++ show p
                                     if p 
                                             then do 
                                                     c <- getChildProcessesFromPid pid 
                                                     if length c == 1
                                                             then getExeWorkingPathFromPid $ Pid $ read $ head c
                                                             else EitherT $ return $ Left $ Err $ (0, "")
                                             else EitherT $ return $ Left $ Err $ (0, "")
                                           

isPidProcessExecProcess :: Pid 
                        -> String  -- Process (just name of executable not full path)
                        -> IO Bool
isPidProcessExecProcess pid exec =  do
                                       a <- runEitherT $ getExePathFromPid pid 
                                       b <- runEitherT $ getExePathFromExe exec

                                       if Data.Either.isRight a && Data.Either.isRight b
                                        then return (a == b)
                                        else return False



-- uses which and realpath to get the path of an exe
-- $ realpath $(which <name of exe>)
getExePathFromExe :: String -> EitherT Err IO StdOut
getExePathFromExe exe = execAndReadProcess "which" [exe] ""             >>=
                \x  -> execAndReadProcess "realpath" [x] ""             >>=
                \x' -> return . strip $ x'

-- uses which and realpath to get the path of an exe
-- $ readlink /proc/<PID>/exe
getExePathFromPid :: Pid -> EitherT Err IO StdOut
getExePathFromPid pid = strip <$> execAndReadProcess "readlink" ["/proc/"++ show pid ++ "/exe"] ""

-- uses which and realpath to get the path of an exe
-- $ readlink /proc/<PID>/cwd
getExeWorkingPathFromPid :: Pid -> EitherT Err IO StdOut
getExeWorkingPathFromPid pid = strip <$> execAndReadProcess "readlink" ["/proc/"++ show pid ++ "/cwd"] ""

-- gets the PID of the child process
-- $ pgrep -P <int of the process>
getChildProcessesFromPid :: Pid -> EitherT Err IO [StdOut]
getChildProcessesFromPid pid = execAndReadProcess "pgrep" ["-P", show pid] "" >>= (return . lines)

{- readProcessExitCode -}
type Exec = String
type Arg = String
type StdIn = String
type StdOut = String

type ErrCode = Int
type StdErr = String

newtype Err = Err (ErrCode, StdErr) deriving Eq
instance Show Err where
        show (Err (code, stderr)) = "Error code: " ++ show code ++ "\n StdErr: " ++ stderr

execAndReadProcess :: Exec -> [Arg] -> StdIn -> EitherT Err IO StdOut
execAndReadProcess exec args stdin = EitherT $ runProcessWithInputWithErrorCode exec args stdin >>= 
                                        (\case 
                                            (GHC.IO.Exception.ExitSuccess, stdout, _) 
                                                            -> return $ Right stdout 
                                            (GHC.IO.Exception.ExitFailure n, _, stderr) 
                                                            ->  return $ Left $ Err (n, stderr)
                                        )                                 
-- | Returns the output and stdErr
runProcessWithInputWithErrorCode :: MonadIO m => Exec -> [Arg] -> StdIn -> m (GHC.IO.Exception.ExitCode, StdOut, StdErr)
runProcessWithInputWithErrorCode cmd args input = io $ do
        (pin, pout, perr, phand) <- System.Process.runInteractiveProcess (Codec.Binary.UTF8.String.encodeString cmd)
                                            (map Codec.Binary.UTF8.String.encodeString args) Nothing Nothing
        System.IO.hPutStr pin input
        System.IO.hClose pin
        output <- System.IO.hGetContents pout
        erroutput <- System.IO.hGetContents perr

        Control.Monad.when (output == output && erroutput == erroutput) $ return ()
        merrcode <- System.Process.getProcessExitCode phand
        let errcode = case merrcode of 
                        (Just x) -> x
                        Nothing -> GHC.IO.Exception.ExitSuccess

        System.IO.hClose pout
        System.IO.hClose perr
        -- no need to waitForProcess, we ignore SIGCHLD
        return (errcode, output, erroutput)

{- EitherT -}
newtype EitherT e m a = EitherT { runEitherT :: m (Either e a) }

instance Functor f => Functor (EitherT e f) where
        fmap f (EitherT n) = EitherT $ fmap f' n
                where 
                        f' (Left e) = Left e
                        f' (Right a) = Right $ f a 

instance Applicative f => Applicative (EitherT e f) where
        pure n = EitherT $ pure . Right $ n 
        (EitherT f) <*> (EitherT n) = EitherT $ f' <*> n
                where
                        f' = (<*>) <$> f

instance Monad m => Monad (EitherT e m) where
        return = pure
        (EitherT n) >>= f = EitherT $ n >>= (\case
                                                Left a -> return $ Left a
                                                Right a -> runEitherT $ f a
                                            )

liftEitherT :: Monad m => m a -> EitherT e m a
liftEitherT n = EitherT $ n >>= (return . Right)

{- MaybeT -}
newtype MaybeT m a = MaybeT { runMaybeT :: m (Maybe a) }

instance Functor f => Functor (MaybeT f) where
        fmap :: (a -> b) -> MaybeT f a -> MaybeT f b
        fmap f n = MaybeT $ (f <$>) <$> runMaybeT n

instance Applicative f => Applicative (MaybeT f) where
        pure :: Applicative f => a -> MaybeT f a
        pure = MaybeT . pure . Just

        (<*>) (MaybeT f) (MaybeT n) = MaybeT $ fmap (<*>) f <*> n

instance Monad m => Monad (MaybeT m) where
        return = pure
        (>>=) n f = let n' = runMaybeT n
                        in MaybeT $ n' >>= (\case 
                                                (Just x') -> runMaybeT $ f x'
                                                Nothing -> return Nothing)

