import System.Random
import Control.Monad

data Npc = Npc {        kind :: String
               ,          hp :: Int
               ,          ac :: Int
               ,         atk :: Int
               ,         dmg :: String
               ,        move :: Int
               ,      morale :: Int
               ,      skills :: Int
               ,       saves :: Int
               , description :: String }

npc :: String -> Int -> Int -> Int -> String -> Int -> Int -> Int -> Int -> String -> Npc
npc n x = Npc n x

showNpc :: Npc -> String
showNpc (Npc a b c d e f g h i j) = "NPC: " ++ a
              ++ ", hp: " ++ show (b)
              ++ ", ac: " ++ show (c)
              ++ ", atk: +" ++ show (d)
              ++ ", dmg: " ++ e
              ++ ", move: " ++ show (f) ++ "m"
              ++ ", morale: " ++ show (g)
              ++ ", skills: +" ++ show (h)
              ++ ", saves: " ++ show (i) ++ "+"
              ++ "\n" ++ j

instance Show Npc where show = showNpc

-- hit dice = 6
npcWithRandomHp 0 g = npc "peaceful human" (sum $ dice 1 6 g) 10 0 "1d2" 10 6  1 15 "an unarmed ordinary person"
npcWithRandomHp 1 g = npc "martial human" (sum $ dice 2 6 g) 10 1 "1d4" 10 8  1 15 "someone's been going to the gym, and has a knife"
npcWithRandomHp 2 g = npc "veteran fighter" (sum $ dice 1 6 g) 14 2 "1d6+1" 10 9  1 14 "an unarmed ordinary person"
npcWithRandomHp 3 g = npc "elite fighter" (sum $ dice 3 6 g) 16 4 "1d6+1" 10 10 2 14 "a gun for hire, wearing combat armor"
npcWithRandomHp 4 g = npc "heroic fighter" (sum $ dice 6 6 g) 16 8 "1d8+3" 10 11 3 12 "a trained killer, wearing combat armor"

-- roll nDx dice
dice :: (RandomGen g) => Int -> Int -> g -> [Int]
dice n x = take n . randomRs (1, x)

main :: IO ()
main = do
  putStrLn ("How many enemies do you want?")
  input <- getLine
  let n = (read input :: Int)
  putStrLn ("Enter an integer for enemy difficulty [0-4]")
  input <- getLine
  let x = (read input :: Int)
  replicateM_ n $ do
    enemy <- liftM (npcWithRandomHp x) newStdGen
    putStrLn $ (show (enemy))
