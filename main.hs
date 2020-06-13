-- stack runghc
module Main where
-- Always captial for the start
-- stack similar to pyenv 


-- main, entrypoint 
-- Comment
-- Main function

main :: IO ()
main = do 
    putStrLn "hello world"
    putStrLn "hello world"
    putStrLn "hello world"
    putStrLn "hello world"
    putStrLn "hello world"

-- "Alpha" <> "Beta"
-- (<>) "Alpha" "Beta"
concatAlpha :: String -> String
concatAlpha = ((<>) "Alpha" :: String -> String)