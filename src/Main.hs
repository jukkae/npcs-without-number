import System.Random

data Npc = Npc { name :: String,
                   hp :: Int } deriving (Show)

npc :: Int -> Npc
npc x =
  Npc "enemy" x

main :: IO ()
main = do
  g <- newStdGen -- seed random generator
  putStrLn ("enter an integer for enemy type")
  input <- getLine
  let x = (read input :: Int)
  putStrLn ("NPC: " ++ name (npc x) ++ ", hp: " ++ show (hp (npc x)))
  print $ take 1 (randomRs ('1', '6') g)
