module AcceptanceSpec where

import Test.Hspec

spec :: Spec
spec = do

  describe "Mars Rover" $ do

    it "should print the statement after some money movements" $ do
      1 `shouldBe` 1
--      readerT of Either

--      type ParseCommand = a -> Either MarsRoverError Command
--      let initialPosition = Position (0,0) N
--      let grid = Grid (5,5)
--      let initialRover = Rover { position = initialPosition, obstacle = Nothing }
--      let moveRover = move parseCommand
--      let finalRover = moveRover initialRover grid "ffflffrb"
--
--      finalRover `shouldBe` Right $ Rover { currentPosition = Position }