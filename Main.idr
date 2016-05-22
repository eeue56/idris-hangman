module Main

import Hangman
import Messages

wantToKeepPlaying : IO Bool
wantToKeepPlaying = do
  putStrLn Messages.doYouWantToPlay
  yes <- getChar
  return (yes == 'y')

nextLetter : GuessedWord -> IO Char
nextLetter word = do
  putStrLn $ Messages.currentWord word
  putStrLn $ Messages.enterALetter $ guessedLetters word
  char <- getChar
  return (char)


nextInteration : Game -> IO Game
nextInteration game =
  case currentWord game of
    Nothing =>
      do
        return (game)
    Just word =>
      if (livesLeft game < 0 || (isAllGuessed $ word) ) then
        do
          putStrLn Messages.death
          return (game)
      else
        do
          letter <- nextLetter $ word
          nextInteration (makeGuess game letter)

mainLoop : Game -> IO Game
mainLoop game = do
  putStrLn (Messages.wordsLeft game)
  wantsToPlay <- wantToKeepPlaying

  when (not wantsToPlay || (not $ anyWordsLeft game) ) $ do
    putStrLn $ show wantsToPlay
    putStrLn Messages.goodbye
    return ()

  game <- nextInteration (setNextWord game)

  putStrLn "\n\n\n"

  mainLoop game


main : IO ()
main = do
  mainLoop defaultGame
  return ()
