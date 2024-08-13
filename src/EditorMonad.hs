module EditorMonad where

import           Control.Monad.Trans.State.Lazy

tabSpaces :: Int
tabSpaces = 4

data WindowSize = WindowSize {
  height :: Int,
  width  :: Int
}

data CursorPosition = CursorPosition {
  row :: Int,
  col :: Int
}

data Offset = Offset {
  rowOffset :: Int,
  colOffset :: Int
}

data Mode
  = NORMAL
  | INSERT
  | COMMAND
  deriving (Eq)

data CommandLine = CommandLine {
  prompt      :: String,
  input       :: String,
  lastPattern :: String
}

data EditorState = EditorState {
  mode           :: Mode,
  file           :: String,
  windowSize     :: WindowSize,
  cursorPosition :: CursorPosition,
  offset         :: Offset,
  fileLines      :: [String],
  commandLine    :: CommandLine
}

type EditorMonad = StateT EditorState IO

normalizeCursorPosition :: EditorMonad ()
normalizeCursorPosition = do
  CursorPosition { row = r, col = c } <- normalizedCursorPosition
  modify (\s -> s { cursorPosition = CursorPosition { row = r, col = c } })

normalizedCursorPosition :: EditorMonad CursorPosition
normalizedCursorPosition = do
  CursorPosition { row = r, col = c } <- gets cursorPosition
  lines <- gets fileLines
  let line = lines !! r
  return $ CursorPosition { row = r, col = max (min c (length line - 1)) 0 }

putCursorRow :: Int -> EditorMonad ()
putCursorRow r = do
  c <- gets (col . cursorPosition)
  modify (\s -> s { cursorPosition = CursorPosition { row = r, col = c } })

putCursorCol :: Int -> EditorMonad ()
putCursorCol c = do
  r <- gets (row . cursorPosition)
  modify (\s -> s { cursorPosition = CursorPosition { row = r, col = c } })

increaseCursorCol :: EditorMonad ()
increaseCursorCol = do
  c <- gets (col . cursorPosition)
  putCursorCol $ c + 1

putMode :: Mode -> EditorMonad ()
putMode mode = modify (\s -> s { mode = mode })

appendToInput :: Char -> EditorMonad ()
appendToInput c = do
  i <- gets (input . commandLine)
  putInput (i ++ [c])

putPrompt :: String -> EditorMonad ()
putPrompt p = do
  cmdLine <- gets commandLine
  modify (\s -> s { commandLine = cmdLine { prompt = p } })

putInput :: String -> EditorMonad ()
putInput i = do
  cmdLine <- gets commandLine
  modify (\s -> s { commandLine = cmdLine { input = i } })

popFromInput :: EditorMonad ()
popFromInput = do
  input <- gets (input . commandLine)
  let deleteLast []     = []
      deleteLast [_]    = []
      deleteLast (x:xs) = x : deleteLast xs
  putInput (deleteLast input)

putLastPattern :: String -> EditorMonad ()
putLastPattern p = do
  cmdLine <- gets commandLine
  modify (\s -> s { commandLine = cmdLine { lastPattern = p } })

putOffsetRow :: Int -> EditorMonad ()
putOffsetRow ro = do
  off <- gets offset
  modify (\s -> s { offset = off { rowOffset = ro } })

putOffsetCol :: Int -> EditorMonad ()
putOffsetCol co = do
  off <- gets offset
  modify (\s -> s { offset = off { colOffset = co } })
