module Operations where

import           Control.Monad.Trans.State.Lazy
import           EditorMonad

moveCursor :: Char -> EditorMonad ()
moveCursor ch = do
  let shouldNormalize = ch `elem` ['h', 'l']
  CursorPosition { row = r, col = c } <- if shouldNormalize then normalizedCursorPosition else gets cursorPosition
  lines <- gets fileLines
  let line = lines !! r
  case ch of
    'j' -> putCursorRow $ min (r + 1) (length lines - 1)
    'k' -> putCursorRow $ max (r - 1) 0
    'h' -> putCursorCol $ max (c - 1) 0
    'l' -> putCursorCol $ min (c + 1) (max (length line - 1) 0)
    '0' -> putCursorCol 0
    '$' -> putCursorCol $ max (length line - 1) 0
    _   -> error "internal error"

modifyLine :: (String -> EditorMonad [String]) -> EditorMonad ()
modifyLine modifier = do
  r <- gets (row . cursorPosition)
  lines <- gets fileLines
  modifiedLines <- mapM (\(i, line) -> if i /= r then return [line] else modifier line) (zip [0..] lines)
  modify (\s -> s { fileLines = concat modifiedLines })

insertChar :: Char -> String -> EditorMonad [String]
insertChar ch line = do
  c <- gets (col . cursorPosition)
  putCursorCol $ c + 1
  return $ [take c line ++ [ch] ++ drop c line]

deleteChar :: String -> EditorMonad [String]
deleteChar line = do
  c <- gets (col . cursorPosition)
  putCursorCol $ max (c - 1) 0
  return $ [take (max (c - 1) 0) line ++ drop c line]

splitLine :: String -> EditorMonad [String]
splitLine line = do
  CursorPosition { row = r, col = c } <- gets cursorPosition
  modify (\s -> s { cursorPosition = CursorPosition { row = r + 1, col = 0 } })
  return $ [take c line, drop c line]
