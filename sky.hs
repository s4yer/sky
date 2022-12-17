module Main (main) where

import Data.List as L
import Data.Map as M
import System.Environment (getArgs)
import System.Random (randomRIO)
import System.Console.ANSI (clearScreen)
import Control.Monad (foldM_)
import Control.Concurrent (threadDelay)

newtype World = World (Map (Int,Int) Life)
                deriving (Show)

data Life = Alive
          | Dead
          deriving (Eq,Show)

newWorld :: Int -> Int -> [(Int,Int)] -> World
newWorld x y ts = initWorld ts . World . M.fromList . toTuple . coordinates x $ y
  where
    -- 座標を出力
    coordinates' :: Int -> Int -> [(Int,Int)]
    coordinates' x y = concat . L.zipWith (\x ys -> zip (L.repeat x) ys) [0..x] . L.repeat $ [0..y]
    
    -- 別解
    coordinates :: Int -> Int -> [(Int,Int)]
    coordinates x y = do
      x <- [0..x]
      y <- [0..y]
      return (x,y)
    
    toTuple :: [(Int,Int)] -> [((Int,Int),Life)]
    toTuple = L.map (\t -> (t,Dead))

    oneStep :: World -> (Int,Int) -> World
    oneStep (World a) b = World (M.update (\_ -> Just Alive) b a)

    initWorld :: [(Int,Int)] -> World -> World
    initWorld ts w = L.foldl oneStep w ts

clearWorld :: Life -> World -> World
clearWorld life = mapWorld (\_ _ -> life)

at :: Int -> Int -> World -> Maybe Life
at x y (World w) = M.lookup (x,y) w

mapWorld :: ((Int,Int) -> Life -> Life) -> World -> World
mapWorld f (World w) = World . M.mapWithKey f $ w

updateWorld :: World -> World
updateWorld _world = mapWorld toNextLife _world
  where
    addTuple :: (Int,Int) -> (Int,Int) -> (Int,Int)
    addTuple (x,y) (x',y') = (x+x',y+y')

    aroundKeys :: (Int,Int) -> [(Int,Int)]
    aroundKeys t = L.map (addTuple t) [(-1,-1),(-1,0),(-1,1),(0,1),(1,1),(1,0),(1,-1),(0,-1)]

    aroundAliveCount :: (Int,Int) -> Int
    aroundAliveCount = length . L.filter (==Just Alive) . L.map (\(x,y) -> at x y _world) . aroundKeys

    -- 冗長な記述だがグラフ簡約が働く
    isNextAlive :: (Int,Int) -> Life -> Bool
    isNextAlive k v | v == Alive = aroundAliveCount k == 2 ||
                                   aroundAliveCount k == 3
                    | v == Dead  = aroundAliveCount k == 3
    
    -- mapWorldに渡す関数。キーと対応する値を受け取る。
    toNextLife :: (Int,Int) -> Life -> Life
    toNextLife k v = if isNextAlive k v
                     then Alive
                     else Dead

renderWorld :: World -> String
renderWorld = unlines . toStrings . toLifes . groupX
  where
    groupX :: World -> [[((Int,Int),Life)]]
    groupX (World w) = L.transpose . L.groupBy (\((x,_),_) ((x',_),_) -> x == x') . M.toList $ w

    toLifes :: [[((Int,Int),Life)]] -> [[Life]]
    toLifes = L.map (\xs -> L.map snd xs)

    toStrings :: [[Life]] -> [String]
    toStrings = L.map (\xs -> L.map (\life -> if life == Alive then '*' else '_') xs)

-- do記法内での再帰
genRandomPositions :: (Int,Int) -> Int -> IO [(Int,Int)]
genRandomPositions (maxCol,maxRow) = loop
  where
    loop :: Int -> IO [(Int,Int)]
    loop n = do
      if n == 0
      then do
        return []
      else do
        x <- randomRIO (0,maxCol) ::IO Int
        y <- randomRIO (0,maxRow) ::IO Int
        next <- loop (n-1)
        return ((x,y) : next)

test = do
  let x = 2
  let y = 2
  let newWorld' = newWorld x y
  let putStrLn' = putStrLn . renderWorld
  
  -- Wikipediaの例を使う
  let w = newWorld' [(0,0),(1,0),(0,1)]
  let w2 = newWorld' [(1,0),(2,0),(1,1),(2,1)]
  let w3 = newWorld' [(1,1),(2,1)]
  let w4 = newWorld' [(0,0),(1,0),(2,0),(0,1),(1,1)]
  putStrLn "----- Current -----"
  putStrLn' w
  putStrLn' w2
  putStrLn' w3
  putStrLn' w4

  putStrLn "----- Next -----"
  putStrLn' . updateWorld $ w
  putStrLn' . updateWorld $ w2
  putStrLn' . updateWorld $ w3
  putStrLn' . updateWorld $ w4

main :: IO ()
main = do
  args <- getArgs
  
  let (x,y,r,t) = if length args == 4
                  then (read $ args !! 0 ::Int,
                        read $ args !! 1 ::Int,
                        read $ args !! 2 ::Int,
                        (read $ args !! 3 ::Int) * 1000)
                  else if length args == 3
                       then (read $ args !! 0 ::Int,
                             read $ args !! 1 ::Int,
                             read $ args !! 2 ::Int,
                             200 * 1000)
                       else (100,30,500,200 * 1000)
  
  rand <- genRandomPositions (x,y) r
  let w = newWorld x y rand
  foldM_ aLoop w $ repeat t
  return ()
  where
    aLoop :: World -> Int -> IO World
    aLoop w t = do
      clearScreen
      putStrLn $ renderWorld w
      threadDelay t  -- micro seconds
      return $ updateWorld w
