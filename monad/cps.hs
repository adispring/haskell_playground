import Control.Monad
import Control.Monad.Trans.Class
import Control.Monad.Trans.Cont
import Data.Char

-- We assume some primitives add and square for the example:

add :: Int -> Int -> Int
add x y = x + y

square :: Int -> Int
square x = x * x

pythagoras :: Int -> Int -> Int
pythagoras x y = add (square x) (square y)

add_cps :: Int -> Int -> ((Int -> r) -> r)
add_cps x y = \k -> k (add x y)

square_cps :: Int -> ((Int -> r) -> r)
square_cps x = \k -> k (square x)

pythagoras_cps :: Int -> Int -> ((Int -> r) -> r)
pythagoras_cps x y = \k ->
 square_cps x $ \x_squared ->
 square_cps y $ \y_squared ->
 add_cps x_squared y_squared $ k

thrice_cps :: (a -> ((a -> r) -> r)) -> a -> ((a -> r) -> r)
thrice_cps f_cps x = \k ->
 f_cps x $ \fx ->
 f_cps fx $ \ffx ->
 f_cps ffx $ k

add_cont :: Int -> Int -> Cont r Int
add_cont x y = return (add x y)

square_cont :: Int -> Cont r Int
square_cont x = return (square x)

pythagoras_cont :: Int -> Int -> Cont r Int
pythagoras_cont x y = do
    x_squared <- square_cont x
    y_squared <- square_cont y
    add_cont x_squared y_squared

foo :: Int -> Cont r String
foo x = callCC $ \k -> do
  let y = x ^ 2 + 3
  when (y > 20) $ k "greater than 20"
  return (show $ y - 4)

bar :: Char -> String -> Cont r Int
bar c s = do
    msg <- callCC $ \k -> do
        let s0 = c : s
        when (s0 == "你好") $ k "他们说你好呀"
        let s1 = show s0
        return ("他们似乎在说: " ++ s1)
    return (length msg)

fun :: Int -> String
fun n = (`runCont` id) $ do
    str <- callCC $ \exit1 -> do                            -- define "exit1"
        when (n < 10) (exit1 (show n))
        let ns = map digitToInt (show (n `div` 2))
        n' <- callCC $ \exit2 -> do                         -- define "exit2"
            when ((length ns) < 3) (exit2 (length ns))
            when ((length ns) < 5) (exit2 n)
            when ((length ns) < 7) $ do
                let ns' = map intToDigit (reverse ns)
                exit1 (dropWhile (=='0') ns')               --escape 2 levels
            return $ sum ns
        return $ "(ns = " ++ (show ns) ++ ") " ++ (show n')
    return $ "Answer: " ++ str

divExcpt :: Int -> Int -> (String -> Cont r Int) -> Cont r Int
divExcpt x y handler = callCC $ \ok -> do
  err <- callCC $ \notOK -> do
    when (y == 0) $ notOK "Denominator 0"
    ok $ x `div` y
  handler err

main :: IO()
main = flip runContT return $ do
    lift $ putStrLn "alpha"
    (k, num) <- callCC $ \k -> let f x = k (f, x)
                               in return (f, 0)
    lift $ putStrLn "beta"
    lift $ putStrLn "gamma"
    if num < 5
        then k (num + 1) >> return ()
        else lift $ print num
