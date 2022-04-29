{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}
module MarsRover where

import Control.Monad.Trans.Reader
import Data.Functor

newtype Grid = Grid (Int, Int) deriving (Eq, Show)

data Direction = North | East | South | West deriving (Eq, Show, Enum)

data Position = Position (Int, Int) Direction deriving (Eq, Show)

data Rover = Rover { position :: Position, obstacle :: Maybe Position } deriving (Eq, Show)

data Command =  MoveForward | MoveBackwards | TurnLeft | TurnRight deriving (Eq, Show)

data RoverError = InvalidCommand String deriving (Eq, Show)

type ParseCommands = String -> Either RoverError [Command]

data Dependencies =  Dependencies { parseCommands ::ParseCommands }

run :: Rover -> Grid -> String -> Either RoverError Rover
run rover grid commands = runReaderT (execute rover grid commands) Dependencies { parseCommands = defaultParseCommands }

execute :: Rover -> Grid -> String -> ReaderT Dependencies (Either RoverError) Rover
execute rover grid unparsedCommands = ReaderT (\deps -> let executeCommands commands = foldr (\c r -> executeCommand c r grid) rover commands
                                                            commands = parseCommands deps $ unparsedCommands
                                                        in executeCommands <$> commands)

executeCommand :: Command -> Rover -> Grid -> Rover
executeCommand _ stuck@Rover { obstacle = Just _ } _ = stuck
executeCommand TurnLeft rover@Rover { position = Position xy dir} grid = rover { position = Position xy (turnLeft dir) }
executeCommand TurnRight rover@Rover { position = Position xy dir} grid = rover { position = Position xy (turnRight dir) }
executeCommand MoveForward rover grid = moveOnXAxis rover grid 1
executeCommand MoveBackwards rover grid = moveOnXAxis rover grid (-1)

moveOnXAxis :: Rover -> Grid -> Int -> Rover
moveOnXAxis rover@Rover { position = Position (x,y) dir} grid positions = rover { position = Position (wrap (x, y + positions) grid) dir }

wrap :: (Int, Int) -> Grid -> (Int, Int)
wrap (x, y) (Grid (z, w)) = (x `mod` z,  y `mod` w)

turnLeft North = West
turnLeft direction = pred direction

turnRight West = North
turnRight direction = succ direction

defaultParseCommands :: ParseCommands
defaultParseCommands unparsedCommands = sequenceA (char2Command <$> unparsedCommands)
  where char2Command 'r' = Right TurnRight
        char2Command 'l' = Right TurnLeft
        char2Command 'f' = Right MoveForward
        char2Command 'b' = Right MoveBackwards
        char2Command invalid = Left $ InvalidCommand [invalid]
