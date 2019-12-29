module AsmConv.Util where

import System.Exit

maybeToEither :: a -> Maybe b -> Either a b
maybeToEither _ (Just x) = Right x
maybeToEither m Nothing = Left m

eitherGuard :: a -> Bool -> Either a ()
eitherGuard _ True = Right ()
eitherGuard l False = Left l

eitherToIO :: Either a b -> (a -> String) -> IO b
eitherToIO (Right b) _ = return b
eitherToIO (Left a) f = die $ f a

eitherMap :: (a -> Either s b) -> [a] -> Either (s, a) [b]
eitherMap f l =
    let g a = mapLeft (\ s -> (s, a)) $ f a
    in mapM g l

mapSnd :: (b -> c) -> (a, b) -> (a, c)
mapSnd f (a, b) = (a, f b)

mapLeft :: (a -> c) -> Either a b -> Either c b
mapLeft f = either (Left . f) Right

mapFstM :: (Monad m) => (a -> m c) -> (a, b) -> m (c, b)
mapFstM f (a, b) = f a >>= \ c -> return (c, b)

mapSndM :: (Monad m) => (b -> m c) -> (a, b) -> m (a, c)
mapSndM f (a, b) = f b >>= \ c -> return (a, c)

cartesianProduct :: [a] -> [b] -> [(a, b)]
cartesianProduct as bs = concat $ map (\ a -> map (\ b -> (a, b)) bs) as
