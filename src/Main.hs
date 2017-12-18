import System.Random

data Npc = Npc { name :: String,
                   hp :: Int } deriving (Show)

npc :: String -> Int -> Npc
npc n x = Npc n x

-- hit dice = 6
npcWithRandomHp 0 g = npc "elite fighter" (sum $ dice 3 6 g)
npcWithRandomHp 1 g = npc "basic fighter" (sum $ dice 2 6 g)
npcWithRandomHp x g = npc "enemy" (sum $ dice x 6 g)

-- roll nDx dice
dice :: (RandomGen g) => Int -> Int -> g -> [Int]
dice n x = take n . randomRs (1, x)

main :: IO ()
main = do
  g <- newStdGen -- seed random generator
  putStrLn ("enter an integer for enemy type")
  input <- getLine
  let x = (read input :: Int)
  let n = npcWithRandomHp x g
  putStrLn ("NPC: " ++ name n ++ ", hp: " ++ show (hp n ))
