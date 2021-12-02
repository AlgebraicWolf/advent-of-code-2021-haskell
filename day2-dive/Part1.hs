{-# LANGUAGE RecordWildCards #-}

module Part1 where

import Control.Applicative
import Data.Monoid
import System.Environment (getArgs)
import Text.Blaze.Html5.Attributes (contenteditable)
import System.Process.Internals (CreateProcess(cmdspec))

data Command = Forward Int | Down Int | Up Int deriving Show 

data Coords = Coords {
    coordsDepth :: Int,
    coordsPosition :: Int 
} deriving Show

readMaybe :: (Read a) => String -> Maybe a
readMaybe s = case reads s of
                [(x, "")] -> Just x
                _         -> Nothing

executeCommand :: Command -> Coords -> Coords
executeCommand (Forward x) coords@(Coords { .. }) = coords { coordsPosition = coordsPosition + x }
executeCommand (Down x) coords@(Coords { .. }) = coords { coordsDepth = coordsDepth + x }
executeCommand (Up x) coords@(Coords { .. }) = coords { coordsDepth = coordsDepth - x }

commandsToFunction :: [Command] -> Coords -> Coords
commandsToFunction cmds = appEndo $ mconcat funcs
    where funcs = Endo . executeCommand <$> cmds

parseCommand :: String -> Either String Command
parseCommand ('f':'o':'r':'w':'a':'r':'d':' ':str) = case readMaybe str of
                                                        (Just n) -> Right $ Forward n
                                                        Nothing  -> Left "Invalid argument for 'forward'"
parseCommand ('d':'o':'w':'n':' ':str) = case readMaybe str of
                                            (Just n) -> Right $ Down n
                                            Nothing  -> Left "Invalid argument for 'down'"
parseCommand ('u':'p':' ':str) = case readMaybe str of
                                    (Just n) -> Right $ Up n
                                    Nothing  -> Left "Invalid argument for 'up'"
parseCommand str = Left "Unknown command"

parseCommands :: [String] -> Either String [Command]
parseCommands = mapM parseCommand


showError :: String -> IO ()
showError str = do
    putStr "An error occurred during input parsing: "
    putStrLn str

showAnswer :: [Command] -> IO ()
showAnswer cmds = do
    let func = commandsToFunction cmds
    let endCoords = func (Coords 0 0)
    let answer = coordsPosition endCoords * coordsDepth endCoords
    print answer


main :: IO ()
main = do
    args <- getArgs
    content <- readFile (head args)
    let commandStrs = lines content
    case parseCommands commandStrs of
        (Left err) -> showError err
        (Right cmds) -> showAnswer cmds


    

