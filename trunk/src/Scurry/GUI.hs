module Scurry.GUI (
    gui,
) where

import Control.Concurrent
import Graphics.UI.Gtk

startGui :: IO ()
startGui = do
    unsafeInitGUIForThreadedRTS
    window <- windowNew
    button <- buttonNew

    set window [ containerBorderWidth := 10,
                 containerChild := button ]
    set button [ buttonLabel := "Hello World" ]
    onClicked button (putStrLn "Hello World")
    onDestroy window mainQuit
    widgetShowAll window
    mainGUI

gui :: IO ()
gui = do
    b <- isCurrentThreadBound

    if b then startGui
         else fail "GUI thread must be a bound thread (forkOS)."
