npc :: Int -> [Char]
npc x =
  "enemy"

main :: IO ()
main = do
  putStrLn ("enter an integer for enemy type")
  input <- getLine
  let x = (read input :: Int)
  putStrLn ("NPC: " ++ npc x)
