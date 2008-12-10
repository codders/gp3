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

myDraw :: Render ()
myDraw = 
  do
    setSourceRGB 1 1 0
    setLineWidth 5
  
    moveTo 120 60
    lineTo 60 110
    lineTo 180 110
    closePath
    
    stroke

main :: FilePath -> IO ()
main gladepath = 
  do
    unsafeInitGUIForThreadedRTS
    c_gnome_init
    timeoutAddFull (yield >> return True) priorityDefaultIdle 100
    gui <- loadGlade gladepath
    connectGui gui
    windowPresent (mainApp gui)
    mainGUI

loadGlade gladepath = 
  do
    Just xml <- xmlNew gladepath
    app <- xmlGetWidget xml castToWindow "MainApp"
    canvas <- xmlGetWidget xml castToDrawingArea "GameCanvas"
    onExpose canvas (\x -> do drawWin <- widgetGetDrawWindow canvas
                              renderWithDrawable drawWin myDraw
                              return (eventSent x))
    return $ GUI app undefined

connectGui gui = 
  do
    onDestroy (mainApp gui) mainQuit
