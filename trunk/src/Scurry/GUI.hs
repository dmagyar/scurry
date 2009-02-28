module Scurry.GUI (
    startGui,
) where

import Paths_Scurry

import Control.Concurrent
import Graphics.UI.Gtk
import Graphics.UI.Gtk.Glade

gui :: IO ()
gui = do
    unsafeInitGUIForThreadedRTS

    gladeFile <- getDataFileName "scurry.glade"

    Just xml <- xmlNew gladeFile
    window <- xmlGetWidget xml castToWindow "window"
    onDestroy window mainQuit
    widgetShowAll window
    mainGUI

startGui :: IO ()
startGui = do
    b <- isCurrentThreadBound

    if b then gui
         else fail "GUI thread must be a bound thread (forkOS)."
