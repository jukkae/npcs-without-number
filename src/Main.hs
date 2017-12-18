import System.Random

data Npc = Npc { name :: String,
                   hp :: Int } deriving (Show)

npc :: Int -> Npc
npc x =
  Npc "enemy" x

npcFromType :: Int -> Npc
npcFromType 0 = Npc "elite fighter" 3
npcFromType 1 = Npc "basic fighter" 2
npcFromType x = npc x

-- roll nDx dice
dice :: (RandomGen g) => Int -> Int -> g -> [Int]
dice n x = take n . randomRs (1, x)

main :: IO ()
main = do
  g <- newStdGen -- seed random generator
  putStrLn ("enter an integer for enemy type")
  input <- getLine
  let x = (read input :: Int)
  let n = npcFromType x
  putStrLn ("NPC: " ++ name n ++ ", hp: " ++ show (hp n ))
  print $ dice 1 6 g
