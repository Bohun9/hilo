module Main where

import           Control.Monad                  (when)
import           Control.Monad.Trans.State.Lazy
import           Editor                         (editorLoop)
import           EditorMonad
import           System.Environment             (getArgs)
import           System.Exit                    (exitFailure)
import           System.IO
import           System.Posix.IO
import           System.Posix.Terminal


main :: IO ()
main = do
    args <- getArgs
    if length args /= 1 then do
      hPutStrLn stderr "usage ./hilo <file>"
      exitFailure
    else do
      let filePath = head args
      oldAttr <- getTerminalAttributes stdInput
      let newAttr = foldl withoutMode oldAttr [EnableEcho, ProcessInput, ProcessOutput, ExtendedFunctions, KeyboardInterrupts, StartStopOutput, MapCRtoLF]
      setTerminalAttributes stdInput newAttr Immediately

      windowSize <- getWindowSize ()
      rows <- fileToLines filePath

      evalStateT editorLoop $ EditorState {
        mode = NORMAL,
        file = filePath,
        windowSize = windowSize,
        cursorPosition = CursorPosition { row = 0, col = 0 },
        offset = Offset { rowOffset = 0, colOffset = 0 },
        fileLines = rows,
        commandLine = CommandLine { prompt = "", input = "", lastPattern = "" }
      }

      putStr $ "\x1b[2J" ++ "\x1b[H"
      setTerminalAttributes stdInput oldAttr Immediately

fileToLines :: String -> IO [String]
fileToLines f = do
  content <- readFile f
  return $ if not (null content) then lines content else [""]

getWindowSize :: () -> IO WindowSize
getWindowSize () = do
  putStr $ "\x1b[999B" ++ "\x1b[999C" ++ "\x1b[6n"
  hFlush stdout
  let readResponse = do h <- getChar
                        if h == 'R' then
                          return ""
                        else do
                          t <- readResponse
                          return $ h : t
  response <- readResponse
  when (response !! 0 /= '\x1b') (error "")
  when (response !! 1 /= '[') (error "")
  let (s1, s2) = break (== ';') (drop 2 response)
      (n1, n2) = (read s1, read (drop 1 s2))
  return WindowSize { height = n1 - 1, width = n2 - 2 }
