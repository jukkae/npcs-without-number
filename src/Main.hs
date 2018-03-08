import System.Random
import Control.Monad

data Npc = Npc {   kind :: String
               ,     hp :: Int
               ,     ac :: Int
               ,    atk :: Int
               ,    dmg :: Int
               ,   move :: Int
               , morale :: Int
               , skills :: Int
               ,  saves :: Int }

npc :: String -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> Npc
npc n x = Npc n x

showNpc :: Npc -> String
showNpc (Npc a b c d e f g h i) = "NPC: " ++ a
              ++ ", hp: " ++ show (b)
              ++ ", ac: " ++ show (c)
              ++ ", atk: +" ++ show (d)
              ++ ", dmg: " ++ show (e)
              ++ ", move: " ++ show (f) ++ "m"
              ++ ", morale: " ++ show (g)
              ++ ", skills: +" ++ show (h)
              ++ ", saves: " ++ show (i) ++ "+"

instance Show Npc where show = showNpc

-- hit dice = 6
npcWithRandomHp 0 g = npc "elite fighter" (sum $ dice 3 6 g) 16 4 9 10 10 2 14
npcWithRandomHp 1 g = npc "basic fighter" (sum $ dice 2 6 g) 14 2 6 10 9  1 14
npcWithRandomHp x g = npc "enemy"         (sum $ dice x 6 g) 10 0 3 10 8  1 15

-- roll nDx dice
dice :: (RandomGen g) => Int -> Int -> g -> [Int]
dice n x = take n . randomRs (1, x)

main :: IO ()
main = do
  putStrLn ("How many enemies do you want?")
  input <- getLine
  let n = (read input :: Int)
  putStrLn ("Enter an integer for enemy type [0-2]")
  input <- getLine
  let x = (read input :: Int)
  replicateM_ n $ do
    enemy <- liftM (npcWithRandomHp x) newStdGen
    putStrLn $ (show (enemy))
