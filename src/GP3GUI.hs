{-# LANGUAGE ForeignFunctionInterface,CPP #-}

module GP3GUI where

import Graphics.UI.Gtk
import Graphics.UI.Gtk.Glade
import Graphics.Rendering.Cairo

import Control.Concurrent

foreign import ccall unsafe "gtk_docker.h do_gnome_init"
      c_gnome_init :: IO ()

data GUI = GUI {
    mainApp :: Window,
    canvas :: DrawingArea
  }

main :: FilePath -> IO ()
main gladepath = 
  do
    unsafeInitGUIForThreadedRTS
    c_gnome_init
    timeoutAddFull (yield >> return True) priorityDefaultIdle 100
    gui <- loadGlade gladepath
    connectGui gui
    putStrLn "Gui Connected"
    windowPresent (mainApp gui)
    putStrLn "Window presented"
    mainGUI

loadGlade gladepath = 
  do
    Just xml <- xmlNew gladepath
    app <- xmlGetWidget xml castToWindow "MainApp"
    --canvas <- xmlGetWidget xml castToDrawingArea "GameCanvas"
    return $ GUI app undefined

connectGui gui = 
  do
    onDestroy (mainApp gui) mainQuit
