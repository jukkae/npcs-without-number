data Npc = Npc { name :: String,
                   hp :: Int } deriving (Show)

npc :: Int -> Npc
npc x =
  Npc "enemy" x

main :: IO ()
main = do
  putStrLn ("enter an integer for enemy type")
  input <- getLine
  let x = (read input :: Int)
  putStrLn ("NPC: " ++ name (npc x) ++ ", hp: " ++ show (hp (npc x)))
