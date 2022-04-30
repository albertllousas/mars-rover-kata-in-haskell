{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}
module MarsRover where

import Control.Monad.Trans.Reader
import Data.Functor

data Grid = Grid Size [Obstacle] deriving (Eq, Show)

data Direction = North | East | South | West deriving (Eq, Show, Enum)

data Position = Position Point Direction deriving (Eq, Show)

data Rover = Rover { position :: Position, obstacle :: Maybe Obstacle } deriving (Eq, Show)

data Command =  MoveForward | MoveBackwards | TurnLeft | TurnRight deriving (Eq, Show)

data RoverError = InvalidCommand String deriving (Eq, Show)

type ParseCommands = String -> Either RoverError [Command]

type Obstacle = (Int, Int)

type Point = (Int, Int)

type Size = (Int, Int)

data Dependencies =  Dependencies { parseCommands ::ParseCommands }

run :: Rover -> Grid -> String -> Either RoverError Rover
run rover grid commands = runReaderT (execute rover grid commands) Dependencies { parseCommands = defaultParseCommands }

execute :: Rover -> Grid -> String -> ReaderT Dependencies (Either RoverError) Rover
execute rover grid unparsedCommands = ReaderT (\deps -> let executeCommands commands = foldl (\r c -> executeCommand c r grid) rover commands
                                                            commands = parseCommands deps $ unparsedCommands
                                                        in executeCommands <$> commands)

executeCommand :: Command -> Rover -> Grid -> Rover
executeCommand _ stuck@Rover { obstacle = Just _ } _ = stuck
executeCommand TurnLeft rover@Rover { position = Position point dir} grid = rover { position = Position point (turnLeft dir) }
executeCommand TurnRight rover@Rover { position = Position point dir} grid = rover { position = Position point (turnRight dir) }
executeCommand MoveForward rover grid = case move (position rover) grid 1 of Left o -> rover { obstacle = Just o }; Right p -> rover { position = p }
executeCommand MoveBackwards rover grid = case move (position rover) grid (-1) of Left o -> rover { obstacle = Just o }; Right p -> rover { position = p }

move :: Position -> Grid -> Int -> Either Obstacle Position
move (Position (x,y) North) (Grid size obstacles) n = checkObstacle (Position (wrap (x, y + n) size) North) obstacles
move (Position (x,y) South) (Grid size obstacles) n = checkObstacle (Position (wrap (x, y - (n)) size) South) obstacles
move (Position (x,y) East) (Grid size obstacles) n = checkObstacle (Position (wrap (x + n, y) size) East) obstacles
move (Position (x,y) West) (Grid size obstacles) n = checkObstacle (Position (wrap (x - (n), y) size) West) obstacles

checkObstacle (Position point dir) obstacles = if(point `elem` obstacles) then Left point else Right (Position point dir)

wrap :: Point -> Size -> Point
wrap (x, y) (z, w) = (x `mod` z,  y `mod` w)

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
