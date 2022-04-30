module MarsRoverSpec where

import Test.Hspec
import MarsRover as Rover
import Control.Monad.Trans.Reader
import Data.List.Split

spec :: Spec
spec = do

  let initialRover = Rover { position = (Position (0,0) North), obstacle = Nothing }
  let grid = Grid (5,5) []

  describe "Run commands " $ do

    it "should turn left" $ do
      let rover = Rover.run initialRover grid "l"
      rover `shouldBe` Right (Rover { position = (Position (0,0) West), obstacle = Nothing })

    it "should turn right" $ do
      let rover = Rover.run initialRover grid "r"
      rover `shouldBe` Right (Rover { position = (Position (0,0) East), obstacle = Nothing })

    it "should move forward" $ do
      let rover = Rover.run initialRover grid "f"
      rover `shouldBe` Right (Rover { position = (Position (0,1) North), obstacle = Nothing })

    it "should move backwards" $ do
      let rover = Rover.run Rover { position = (Position (4,4) North), obstacle = Nothing } grid "b"
      rover `shouldBe` Right (Rover { position = (Position (4,3) North), obstacle = Nothing })

    it "should run a sequence of commands" $ do
      let rover = Rover.run initialRover grid "rflffb"
      rover `shouldBe` Right (Rover { position = (Position (1,1) North), obstacle = Nothing })

  describe "Grid wrap" $ do

    it "should move from one edge of the grid to another when the rover goes over the boundaries of the grid" $ do
      let roverInTheEdge = Rover { position = (Position (1,4) North), obstacle = Nothing }
      let rover = Rover.run roverInTheEdge grid "f"
      rover `shouldBe` Right (Rover { position = (Position (1,0) North), obstacle = Nothing })

  describe "Obstacle detection" $ do

    it "should move up to the last possible point when there is an obstacle in the way and report it" $ do
      let gridWithObstacles = Grid (5,5) [(0,4)]
      let rover = Rover.run initialRover gridWithObstacles "fffffff"
      rover `shouldBe` Right (Rover { position = (Position (0,3) North), obstacle = Just (0, 4) })

  describe "Default command parsing" $ do

    it "should parse 'l' as turn left command" $ do
      defaultParseCommands "l" `shouldBe` Right [TurnLeft]

    it "should parse 'r' as turn right command" $ do
      defaultParseCommands "r" `shouldBe` Right [TurnRight]

    it "should parse 'f' as move forward command" $ do
      defaultParseCommands "f" `shouldBe` Right [MoveForward]

    it "should parse 'b' as move backward command" $ do
      defaultParseCommands "b" `shouldBe` Right [MoveBackwards]

    it "should fail parsing an invalid string as a command" $ do
      defaultParseCommands "x" `shouldBe` Left (InvalidCommand "x")

  describe "Execute commands providing a different format" $ do

    let differentParseCommands unparsedCommands = sequenceA (parseCommand <$>  (splitOn "#" unparsedCommands))
          where parseCommand "FORWARD" = Right MoveForward
                parseCommand "BACKWARDS" = Right MoveBackwards
                parseCommand "LEFT" = Right TurnLeft
                parseCommand "RIGHT" = Right TurnRight
                parseCommand invalid = Left $ InvalidCommand invalid

    it "should execute commands" $ do
      let rover = Rover.execute initialRover grid "RIGHT#FORWARD#LEFT#FORWARD#FORWARD#BACKWARDS"
      let result = runReaderT rover Dependencies { parseCommands = differentParseCommands }
      result `shouldBe` Right (Rover { position = (Position (1,1) North), obstacle = Nothing })