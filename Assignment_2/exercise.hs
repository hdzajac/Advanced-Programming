module Exercise(
  binary_random_response,
  random_response,
  binary_response_monad,
  toss,
  randomSt
  -- toss'
  ) where

import qualified System.Random as R
import qualified Control.Monad.State as S

binary_random_response :: R.StdGen -> Bool -> (Bool, R.StdGen)
binary_random_response g true_answer =
  if first_coin == False then (true_answer, g'')
  else (second_coin, g'')
  where (first_coin, g') = R.random g
        (second_coin, g'') = R.random g'

random_response :: R.StdGen -> a -> [a] -> (a, R.StdGen)
random_response g true_answer responses =
  if first_coin == False then (true_answer, g'')
  else (responses !! (second_coin `mod` (length responses)), g'')
  where (first_coin, g') = R.random g
        (second_coin, g'') = R.random g'


random_monad :: (R.RandomGen g) => S.State g Bool
random_monad = S.state R.random  


binary_response_monad :: S.State R.StdGen Bool
binary_response_monad = do
  a <- random_monad
  b <- random_monad
  return b


randomSt :: (R.RandomGen g, R.Random a) => S.State g a  
randomSt = S.state R.random    
  
toss :: S.State R.StdGen (Bool)
toss = do  
    a <- randomSt   
    return (a)


-- toss' :: Bool -> S.State R.StdGen Bool
-- toss' truth = do  
--     (_, val1) <- randomSt
--     if val1 == False then return (snd . randomSt, truth)
--       else return randomSt

