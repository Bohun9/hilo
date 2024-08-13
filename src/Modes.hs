module Modes (processKeyPress) where

import           Control.Monad.IO.Class         (liftIO)
import           Control.Monad.Trans.State.Lazy
import           Data.List
import           EditorMonad
import           Operations

processKeyPressNormal :: Char -> EditorMonad ()
processKeyPressNormal c = do
  case c of
    _ | c `elem` ['j', 'k', 'h', 'l', '0', '$'] ->
        moveCursor c
    _ | c `elem` ['i', 'a'] -> do
        putMode INSERT
        normalizeCursorPosition
        if c == 'a' then do increaseCursorCol else return ()
    _ | c `elem` [':', '/'] -> do
        putMode COMMAND
        putPrompt $ replicate 1 c
        putInput ""
    'n' -> forwardSearch False
    _ -> return ()

processKeyPressInsert :: Char -> EditorMonad ()
processKeyPressInsert '\x1b' =
  do
    putMode NORMAL
    normalizeCursorPosition
processKeyPressInsert '\r' = modifyLine splitLine
processKeyPressInsert '\x7f' = modifyLine deleteChar
processKeyPressInsert c = modifyLine (insertChar c)

forwardSearch :: Bool -> EditorMonad ()
forwardSearch incrementalUsage = do
  pattern <- gets ((if incrementalUsage then input else lastPattern) . commandLine)
  CursorPosition { row = r, col = c } <- normalizedCursorPosition
  lines <- gets fileLines
  let lines' = zip lines [0..]
  let orderedLines = zip ((drop r lines') ++ (take r lines')) ((c + if incrementalUsage then 0 else 1) : repeat 0)
      findFirst text =
        case filter (isPrefixOf pattern) (tails text) of
            []    -> Nothing
            (x:_) -> Just x
      findMatch [] = return ()
      findMatch (((line, lineId), start):xs) =
        case findFirst (drop start line) of
          Just match -> do putCursorRow lineId
                           putCursorCol $ length line - length match
          Nothing -> findMatch xs
  findMatch orderedLines

saveFile :: EditorMonad ()
saveFile = do
  file <- gets file
  lines <- gets fileLines
  let content = concat (map (\line -> line ++ "\n") lines)
  liftIO $ writeFile file content

processKeyPressCommand :: Char -> EditorMonad Bool
processKeyPressCommand '\x1b' = do putMode NORMAL
                                   putPrompt ""
                                   putInput ""
                                   return False
processKeyPressCommand '\r' = do
  CommandLine { prompt = prompt, input = input } <- gets commandLine
  putMode NORMAL
  putPrompt ""
  putInput ""
  cmdLine <- gets commandLine
  case prompt of
    ":" -> do
      case input of
        "w" -> do
                 saveFile
                 putPrompt "File saved"
                 return False
        "q" -> return True
        _ -> do putPrompt "Unknown command"
                return False

    "/" -> do putLastPattern input
              return False

processKeyPressCommand '\x7f' = do
  popFromInput
  return False

processKeyPressCommand c      = do
  appendToInput c
  prompt <- gets $ prompt . commandLine
  if prompt == "/" then forwardSearch True else return ()
  return False

processKeyPress :: EditorMonad Bool
processKeyPress = do
    c <- liftIO getChar
    currentMode <- gets mode
    case currentMode of
      NORMAL  -> do processKeyPressNormal c
                    return False
      INSERT  -> do processKeyPressInsert c
                    return False
      COMMAND -> do processKeyPressCommand c
