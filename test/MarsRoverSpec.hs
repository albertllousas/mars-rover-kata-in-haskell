module MarsRoverSpec where

import Test.Hspec
import MarsRover as Rover
import Control.Monad.Trans.Reader

spec :: Spec
spec = do

  let initialRover = Rover { position = (Position (0,0) North), obstacle = Nothing }
  let grid = Grid (5,5)

  describe "Mars Rover" $ do

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

    describe "Grid wrap" $ do

      it "should move from one edge of the grid to another when the rover goes over the boundaries of the grid" $ do
        let roverInTheEdge = Rover { position = (Position (1,4) North), obstacle = Nothing }
        let rover = Rover.run roverInTheEdge grid "f"
        rover `shouldBe` Right (Rover { position = (Position (1,0) North), obstacle = Nothing })

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

--    describe "Execute commands" $ do
--
--      it "should execute a command to turn right" $ do
--        let rover = Rover.execute initialRover grid "r"
--        let result = runReaderT rover Dependencies { parseCommands = (\_ -> Right [TurnRight]) }
--        result `shouldBe` Right (Rover { position = (Position (0,0) East), obstacle = Nothing })