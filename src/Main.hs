module Main where
import           Control.Monad (forever)
import           Data.Char     (toLower)
import           Data.List     (findIndex, intersperse)
import           Data.Maybe    (isJust)
import           System.Exit   (exitSuccess)
import           System.Random (randomRIO)

data Puzzle =
    Puzzle String [Maybe Char] [Char]

instance Show Puzzle where
    show (Puzzle _ discovered guessed) =
        (intersperse ' ' $ fmap renderPuzzleChar discovered)
        ++ "Guessed so far: " ++ guessed

type WordList = [String]

minWordLength :: Int
minWordLength = 5

maxWordLength :: Int
maxWordLength = 9

renderPuzzleChar :: Maybe Char -> Char
renderPuzzleChar (Just c) = c
renderPuzzleChar Nothing  = '_'

freshPuzzle :: String -> Puzzle
freshPuzzle w =
    Puzzle w (map (\_ -> Nothing) w) []

charInWord :: Puzzle -> Char -> Bool
charInWord (Puzzle w _ _) =
    (flip elem) w

alreadyGuessed :: Puzzle -> Char -> Bool
alreadyGuessed (Puzzle _ _ s) =
    (flip elem) s

updateDiscovered :: Puzzle -> Char -> Puzzle
updateDiscovered (Puzzle w ds gs) c =
    Puzzle w (map (\x -> if c == fst x then Just c else snd x) $ zip w ds) gs

eligibleWords :: WordList -> WordList
eligibleWords =
    filter ((\wl -> (minWordLength <= wl) && (maxWordLength >= wl)) . length)

randomWord :: WordList -> IO String
randomWord wl = do
    idx <- randomRIO (0, length wl)
    return (wl !! idx)

gameWords :: IO WordList
gameWords = do
    words <- allWords
    return $ eligibleWords words

randomWord' :: IO String
randomWord' = gameWords >>= randomWord

allWords :: IO WordList
allWords = do
    dict <- readFile "data/dict.txt"
    return (lines dict)

handleGuess :: Puzzle -> Char -> IO Puzzle
handleGuess puzzle guess = do
    putStrLn $ "Your guess was: " ++ [guess]
    case (charInWord puzzle guess, alreadyGuessed puzzle guess) of
      (_, True) -> do
          putStrLn "You already guessed that \
                   \ character , pick \
                   \ something else!"
          return puzzle
      (True, _) ->  do
          putStrLn "This character was in the \
                   \ word, filling in the word \
                   \ accordingly"
          return (updateDiscovered puzzle guess)
      (False, _) -> do
          putStrLn "This character wasn't in \
                   \the word, try again."
          return (updateGuesses puzzle guess)

updateGuesses :: Puzzle -> Char -> Puzzle
updateGuesses (Puzzle w ds gs) c =
    Puzzle w ds (c : gs)

gameOver :: Puzzle -> IO()
gameOver (Puzzle w _ guessed) =
    if (length guessed) > 7 then
      do putStrLn "You Lose!"
         putStrLn $ "The word was: " ++ w
         exitSuccess
    else return ()

gameWin :: Puzzle -> IO()
gameWin (Puzzle _ filledInSoFar _) =
    if all isJust filledInSoFar then
      do putStrLn "You Win!"
         exitSuccess
      else return ()

runGame :: Puzzle -> IO()
runGame puzzle = forever $ do
    gameOver puzzle
    gameWin puzzle
    putStrLn $ "Current puzzle is: " ++ show puzzle
    putStr "Guess a letter: "
    guess <- getLine
    case guess of
      [c] -> handleGuess puzzle c >>= runGame
      _   -> putStrLn "Your guess must be a single character"

main :: IO ()
main = do
    word <- randomWord'
    let puzzle =
            freshPuzzle (fmap toLower word)
    runGame puzzle
