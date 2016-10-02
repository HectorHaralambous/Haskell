-- starman_words_list.hs

import System.Random

check :: String -> String -> Char -> (Bool, String)
check word display c =
    (c `elem` word, [if x==c
          then c
          else y | (x, y) <- zip word display])


mkguess word display n =
  do putStrLn (display ++ "  " ++ take n (repeat '*'))
     putStr "  Enter your guess: "
     q <- getLine
     let (correct, display') = check word display (q!!0)
     let n' = if correct then n else n-1
     turn word display' n'


turn :: String -> String -> Int -> IO ()
turn word display n =
  do if n==0
       then putStrLn $ "You lose!, it was: " ++ word
       else if word==display
              then putStrLn $ "You Win! It was: " ++ word
              else mkguess word display n

starman :: String -> Int -> IO ()
starman word =
   turn word (replicate (length word) '-')

-- or
-- starman :: String -> Int -> IO ()
-- top-level function. Usage: Start
-- starman word n = turn word ['-' | x <- word] n

start :: IO ()
start =
  do
    content <- readFile "wordlist.txt"
    -- content <- readFile "unixdict.txt"
    let words = lines content
    let difficulty = 3 -- (0-9)
    random_word <- randomRIO (0, length words :: Int)
    -- print(length (words !! random_word))
    starman (words !! random_word) (length (words !! random_word)+difficulty)