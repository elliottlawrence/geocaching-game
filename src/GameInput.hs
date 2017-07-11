{-# LANGUAGE RecordWildCards #-}
module GameInput where

import           Data.Char
import           Graphics.Gloss.Interface.IO.Game
import           System.Exit

import           Constants
import           Types

initialGameInput :: GameInput
initialGameInput =
  [ (leftKey, (Up, 0))
  , (rightKey, (Up, 0))
  , (downKey, (Up, 0))
  , (upKey, (Up, 0))
  , (spaceKey, (Up, 0))
  , (enterKey, (Up, 0))
  ] ++ map (\c -> (Char c, (Up, 0))) ['0'..'9']

leftKey, rightKey, upKey, downKey, spaceKey, enterKey :: Key
leftKey = SpecialKey KeyLeft
rightKey = SpecialKey KeyRight
upKey = SpecialKey KeyUp
downKey = SpecialKey KeyDown
spaceKey = SpecialKey KeySpace
enterKey = SpecialKey KeyEnter

isKeyDown :: Key -> GameInput -> Bool
isKeyDown key gameInput = case lookup key gameInput of
  Just (keyState, time) -> keyState == Down && (time `mod` signalSpeed == 0)
  Nothing -> False

getNumKeyDown :: GameInput -> Maybe Int
getNumKeyDown gameInput = if null levels then Nothing else Just (head levels)
  where levels = [ levelNum | (Char c, (Down, _)) <- gameInput,
                   '0' <= c && c <= '9',
                   let levelNum = if c == '0' then 10 else digitToInt c ]

handleInput :: Event -> Game -> IO Game
handleInput (EventKey (SpecialKey KeyEsc) _ _ _) _ = exitSuccess
handleInput (EventKey key keyState _ _) game@Game{..} = return game {gameInput = gameInput'}
  where gameInput' = [ (k,ks') | (k,ks) <- gameInput, let ks' = if k == key then (keyState, 0) else ks ]
handleInput _ game = return game

updateGameInput :: GameInput -> Bool -> GameInput
updateGameInput gameInput didSignalDie = if didSignalDie
  then initialGameInput
  else [ (k, (ks, time + 1)) | (k, (ks, time)) <- gameInput ]
