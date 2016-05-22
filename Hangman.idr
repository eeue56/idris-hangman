module Hangman

%access export

public export
record GuessedWord where
  constructor NewGuessedWord
  word : String
  guessedLetters : List Char

toEncrypt : List Char -> Char -> Char
toEncrypt known current =
  if elem current known then
    current
  else
    '*'

encryptedWord : GuessedWord -> String
encryptedWord guess =
  pack $ map (toEncrypt knownLetters) allLetters
    where
      allLetters : List Char
      allLetters =
        unpack $ word guess

      knownLetters : List Char
      knownLetters =
        guessedLetters guess

isAllGuessed : GuessedWord -> Bool
isAllGuessed =
  not . elem '*' . unpack . encryptedWord

isAlreadyGuessed : GuessedWord -> Char -> Bool
isAlreadyGuessed guess char =
  elem char (guessedLetters guess)

isInWord : GuessedWord -> Char -> Bool
isInWord guess char =
  elem char $ unpack $ word guess

addNewGuessedLetter : GuessedWord -> Char -> GuessedWord
addNewGuessedLetter guess char =
  record { guessedLetters = char::(guessedLetters guess)} guess

public export
record Game where
  constructor NewGame
  allWords : List String
  currentWord : Maybe GuessedWord
  livesLeft : Int

anyWordsLeft : Game -> Bool
anyWordsLeft =
  not . List.isNil . allWords

setNextWord : Game -> Game
setNextWord game =
  case allWords game of
    [] =>
      record
        { currentWord = Nothing }
        game

    x::xs =>
      record
        { currentWord = Just (NewGuessedWord x []), allWords = xs }
        game


makeGuess : Game -> Char -> Game
makeGuess game char =
  case currentWord game of
    Nothing =>
      game
    Just guess =>
      if isAlreadyGuessed guess char then
        game
      else if isInWord guess char then
        let
          newCurrentWord =
            addNewGuessedLetter guess char
        in
          record { currentWord = Just newCurrentWord } game
      else
        let
          newLives =
            (livesLeft game) - 1
          newCurrentWord =
            addNewGuessedLetter guess char
        in
          record { livesLeft = newLives, currentWord = Just newCurrentWord } game


defaultGame : Game
defaultGame =
  NewGame [ "hello", "cat" ] Nothing 3
