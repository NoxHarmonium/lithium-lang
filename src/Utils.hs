module Utils where

-- Thanks Jan Snajder
-- https://mail.haskell.org/pipermail/beginners/2009-January/000667.html
scanM :: (Monad m) => (a -> b -> m a) -> a -> [b] -> m [a]
scanM f q [] = return [q]
scanM f q (x:xs) = do
    q2 <- f q x
    qs <- scanM f q2 xs
    return (q:qs)

-- Thanks Stephan202
-- http://stackoverflow.com/a/1735541/1153203
middle :: [a] -> [a]
middle l@(_:_:_:_) = middle $ tail $ init l
middle l           = l

third (_, _, x) = x