module Editor where

import           Control.Monad.IO.Class         (liftIO)
import           Control.Monad.Trans.State.Lazy
import           EditorMonad
import           Modes                          (processKeyPress)
import           System.IO
import           Text.Printf                    (printf)

renderLine :: String -> String
renderLine s = concat (map (\c -> if c == '\t' then replicate tabSpaces ' ' else [c]) s)

drawLines :: EditorMonad String
drawLines = do
  WindowSize { height = h, width = w } <- gets windowSize
  Offset { rowOffset = ro, colOffset = co } <- gets offset
  lines' <- gets fileLines
  let lines = take h (drop ro lines')
      makeLine line = take w (drop co (renderLine line))
  let text = concat (map (\line -> "~ " ++ makeLine line ++ "\r\n") lines)
  CommandLine { prompt = prompt, input = input } <- gets commandLine
  let statusBar = (printf "\x1b[%d;1H" (h + 1)) ++ "\x1b[7m" ++ prompt ++ input ++ "\x1b[0m"
  return $ text ++ statusBar

renderCursorPosition :: EditorMonad Int
renderCursorPosition = do
  currentMode <- gets mode
  CursorPosition { row = r, col = c } <- if currentMode == INSERT then gets cursorPosition else normalizedCursorPosition
  lines <- gets fileLines
  let line = lines !! r
  return $ sum (take c (map (\c -> if c == '\t' then tabSpaces else 1) line))

refreshScreen :: EditorMonad ()
refreshScreen = do
  let ansiCodes = "\x1b[?25l" ++ "\x1b[2J" ++ "\x1b[H"
  lines <- drawLines
  currentMode <- gets mode
  r <- gets (row . cursorPosition)
  c <- renderCursorPosition
  Offset { rowOffset = ro, colOffset = co } <- gets offset
  let statusBar = (printf "\x1b[%d;%dH" (r - ro + 1) (c - co + 1 + 2)) ++ "\x1b[?25h"
  liftIO $ putStr $ ansiCodes ++ lines ++ statusBar
  liftIO $ hFlush stdout

scroll :: EditorMonad ()
scroll = do
  s <- get
  c <- renderCursorPosition
  r <- gets (row . cursorPosition)
  Offset { rowOffset = ro, colOffset = co } <- gets offset
  WindowSize { height = h, width = w } <- gets windowSize
  let verticalScroll =
        if r < ro then
          putOffsetRow r
        else if r >= ro + h then
          putOffsetRow $ r - h + 1
        else
          return ()
  let horizontalScroll = do
        if c < co then
          putOffsetCol c
        else if c >= co + w then
          putOffsetCol $ c - w + 1
        else
          return ()
  verticalScroll
  horizontalScroll

editorLoop :: EditorMonad ()
editorLoop = do
  scroll
  refreshScreen
  exit <- processKeyPress
  if exit then
    return ()
  else
    editorLoop
