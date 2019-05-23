module Main where

import Data.Char

main =
    getLine >>=
        \str ->  reclama str
             
            
reclama :: String -> IO ()
reclama =
    \str -> 
      case getWords str of
        str' -> 
            if any (\s -> isFood (map toLower s)) str' 
            then putStrLn "You get 20% off for a dinner in Pasta de la bella!"
            else return ()
            
f :: String -> IO ()
f =
  \str ->  
    if str == ""
    then return ()
    else putStrLn (takeWhile isLetter str) >>
       f (dropWhile (\c -> not (isLetter c)) (dropWhile isLetter str)) 

getWords :: String -> [String]
getWords =
    \str ->
        if str == ""
        then []
        else (takeWhile isLetter str) : 
             getWords (dropWhile (\c -> not (isLetter c)) (dropWhile isLetter str)) 

isFood :: String -> Bool
isFood =
    \str ->
        str == "pizza" || str == "pasta" || str == "coffee"