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
execute rover grid unparsedCommands = ReaderT (\deps -> let executeCommands commands = foldr (\c r -> executeCommand r grid c) rover commands
                                                            commands = parseCommands deps $ unparsedCommands
                                                        in executeCommands <$> commands)

executeCommand :: Rover -> Grid -> Command -> Rover
executeCommand stuck@Rover { obstacle = Just _ } _ _ = stuck
executeCommand rover@Rover { position = Position xy dir} grid TurnLeft = rover { position = Position xy (turnLeft dir) }
executeCommand rover@Rover { position = Position xy dir} grid TurnRight = rover { position = Position xy (turnRight dir) }
executeCommand rover@Rover { position = Position (x,y) dir} grid MoveForward = rover { position = Position (x, y + 1) dir }
executeCommand rover@Rover { position = Position (x,y) dir} grid MoveBackwards = rover { position = Position (x, y - 1) dir }

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
