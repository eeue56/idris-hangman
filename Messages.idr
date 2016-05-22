module Messages

import Hangman

%access export

wordsLeft : Game -> String
wordsLeft game =
  "There are " ++ left ++ " words left."
    where
      left =
        show $ List.length $ allWords game


doYouWantToPlay : String
doYouWantToPlay =
  "Do you want to keep playing?"

currentWord : GuessedWord -> String
currentWord guess =
  "Word so far:" ++ (encryptedWord guess)

enterALetter : List Char -> String
enterALetter [] =
  "So far you've guessed no letters!\nNext letter:"
enterALetter guessedLetters =
  "So far, you have guessed the letters: " ++ (pack guessedLetters) ++ "\nNext letter: "

death : String
death =
  "You ran out of lives!"

goodbye : String
goodbye =
  "Goodbye!"
