{-# LANGUAGE RecordWildCards #-}
module GameInput where

import           Data.Char
import           Graphics.Gloss
import           Graphics.Gloss.Interface.Pure.Game

import           Types

initialGameInput :: GameInput
initialGameInput =
  [ (leftKey, Up)
  , (rightKey, Up)
  , (downKey, Up)
  , (upKey, Up)
  , (spaceKey, Up)
  , (enterKey, Up)
  ] ++ map (\c -> (Char c, Up)) ['0'..'9']

leftKey, rightKey, upKey, downKey, spaceKey, enterKey :: Key
leftKey = SpecialKey KeyLeft
rightKey = SpecialKey KeyRight
upKey = SpecialKey KeyUp
downKey = SpecialKey KeyDown
spaceKey = SpecialKey KeySpace
enterKey = SpecialKey KeyEnter

isKeyDown :: Key -> GameInput -> Bool
isKeyDown key gameInput = case lookup key gameInput of
  Just keyState -> keyState == Down
  Nothing -> False

getNumKeyDown :: GameInput -> Maybe Int
getNumKeyDown gameInput = if null levels then Nothing else Just (head levels)
  where levels = [ levelNum | (Char c, Down) <- gameInput,
                   '0' <= c && c <= '9',
                   let levelNum = if c == '0' then 10 else digitToInt c ]

handleInput :: Event -> Game -> Game
handleInput (EventKey key keyState _ _) game@Game{..} = game {gameInput = gameInput'}
  where gameInput' = [ (k,ks') | (k,ks) <- gameInput, let ks' = if k == key then keyState else ks ]
handleInput _ game = game
