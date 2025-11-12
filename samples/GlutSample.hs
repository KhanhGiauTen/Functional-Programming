{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Graphics.UI.GLUT
import System.IO (hFlush, stdout)

main :: IO ()
main = do
  (_progName, _args) <- getArgsAndInitialize
  initialDisplayMode $= [DoubleBuffered]
  initialWindowSize $= Size 800 600
  _window <- createWindow "Arcane FreeGLUT Sanity Check"
  clearColor $= Color4 0.1 0.1 0.3 (1.0 :: GLfloat)
  displayCallback $= renderFrame
  idleCallback $= Just renderFrame
  mainLoop

renderFrame :: DisplayCallback
renderFrame = do
  clear [ColorBuffer]
  putStrLn "[GLUT] drew a frame"
  hFlush stdout
  flush
  swapBuffers
