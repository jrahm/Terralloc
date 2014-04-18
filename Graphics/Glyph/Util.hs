{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}

module Graphics.Glyph.Util where

import Data.Angle
import Graphics.Rendering.OpenGL
import Data.Maybe
import Data.Char
import Data.Either

import Control.Exception
import Control.Monad

import Data.Foldable as Fold

import Foreign.Ptr
import Foreign.Marshal.Alloc

import Data.Array.MArray

if' :: Bool -> a -> a -> a
if' True a _ = a
if' False _ a = a

(?) :: Bool -> a -> a -> a
(?) = if'

flipIf :: a -> a -> Bool -> a
flipIf a b c = if c then a else b

int :: (Integral a, Num b) => a -> b
int = fromIntegral 

uncurry7 :: (a -> b -> c -> d -> e -> f -> g -> h) -> (a,b,c,d,e,f,g) -> h
uncurry7 func (a,b,c,d,e,f,g) = func a b c d e f g

uncurry6 :: (a -> b -> c -> d -> e -> f -> g) -> (a,b,c,d,e,f) -> g
uncurry6 func (a,b,c,d,e,f) = func a b c d e f

uncurry5 :: (a -> b -> c -> d -> e -> f) -> (a,b,c,d,e) -> f
uncurry5 func (a,b,c,d,e) = func a b c d e

uncurry4 :: (a -> b -> c -> d -> e) -> (a,b,c,d) -> e
uncurry4 func (a,b,c,d) = func a b c d

uncurry3 :: (a -> b -> c -> e) -> (a,b,c) -> e
uncurry3 func (a,b,c) = func a b c

const2 :: a -> b -> c -> a
const2 = const.const

const3 :: a -> b -> c -> d -> a
const3 = const2.const

const4 :: a -> b -> c -> d -> e -> a
const4 = const3.const

gsin :: (Floating a) => a -> a
gsin = sine . Degrees

gcos :: (Floating a) => a -> a
gcos = cosine . Degrees

toEuclidian :: (Floating a) => (a, a, a) -> (a, a, a)
toEuclidian (r, th, ph) = (
        -r * gsin th * gcos ph,
         r * gsin ph,
         r * gcos th * gcos ph
    )

mapT2 :: (a -> b) -> (a,a) -> (b,b) 
mapT2 f (a, b) = (f a, f b)

mapT3 :: (a -> b) -> (a,a,a) -> (b,b,b) 
mapT3 f (a, b, c) = (f a, f b, f c)

mapT4 :: (a -> b) -> (a,a,a,a) -> (b,b,b,b) 
mapT4 f (a, b, c, d) = (f a, f b, f c, f d)

mapT5 :: (a -> b) -> (a,a,a,a,a) -> (b,b,b,b,b) 
mapT5 f (a, b, c, d, e) = (f a, f b, f c, f d, f e)

mapT6 :: (a -> b) -> (a,a,a,a,a,a) -> (b,b,b,b,b,b) 
mapT6 f (a, b, c, d, e, _f) = (f a, f b, f c, f d, f e, f _f)

mapT7 :: (a -> b) -> (a,a,a,a,a,a,a) -> (b,b,b,b,b,b,b) 
mapT7 f (a, b, c, d, e, _f, g) = (f a, f b, f c, f d, f e, f _f, f g)

foldT2 :: (a -> b -> a) -> a -> (b,b) -> a
foldT2 f ini (x,y) = ini `f` x `f` y

foldT3 :: (a -> b -> a) -> a -> (b,b,b) -> a
foldT3 f ini (x,y,z) = ini `f` x `f` y `f` z

foldT4 :: (a -> b -> a) -> a -> (b,b,b,b) -> a
foldT4 f ini (x,y,z,w) = ini `f` x `f` y `f` z `f` w

foldT5 :: (a -> b -> a) -> a -> (b,b,b,b,b) -> a
foldT5 f ini (x,y,z,w,v) = ini `f` x `f` y `f` z `f` w `f` v

tup2Len :: (Real a,Floating b) => (a,a) -> b
tup2Len = sqrt . foldT2 (+) 0 . mapT2 ((**2).toFloating)

tup3Len :: (Real a,Floating b) => (a,a,a) -> b
tup3Len = sqrt . foldT3 (+) 0 . mapT3 ((**2).toFloating)

tup4Len :: (Real a,Floating b) => (a,a,a,a) -> b
tup4Len = sqrt . foldT4 (+) 0 . mapT4 ((**2).toFloating)

tup5Len :: (Real a,Floating b) => (a,a,a,a,a) -> b
tup5Len = sqrt . foldT5 (+) 0 . mapT5 ((**2).toFloating)

expand3 :: a -> (a,a,a)
expand3 t = (t,t,t)

expand4 :: a -> (a,a,a,a)
expand4 t = (t,t,t,t)

expand5 :: a -> (a,a,a,a,a)
expand5 t = (t,t,t,t,t)

expand6 :: a -> (a,a,a,a,a)
expand6 t = (t,t,t,t,t)

zipWithT2 :: (a -> b -> c) -> (a,a) -> (b,b) -> (c,c)
zipWithT2 fu (a, b) (d, e) = (fu a d, fu b e)

zipWithT3 :: (a -> b -> c) -> (a,a,a) -> (b,b,b) -> (c,c,c)
zipWithT3 fu (a, b, c) (d, e, f) = (fu a d, fu b e, fu c f)

zipWithT4 :: (a -> b -> c) -> (a,a,a,a) -> (b,b,b,b) -> (c,c,c,c)
zipWithT4 fu (a, b, c, d) (e, f, g, h) = (fu a e, fu b f, fu c g, fu d h)

zipWithT5 :: (a -> b -> c) -> (a,a,a,a,a) -> (b,b,b,b,b) -> (c,c,c,c,c)
zipWithT5 fu (a, b, c, d, i) (e, f, g, h, j) = (fu a e, fu b f, fu c g, fu d h, fu i j)

toFloating :: (Real a, Floating b) => a -> b
toFloating = fromRational . toRational

(!!%) :: [a] -> Int -> a
(!!%) lst idx = lst !! (idx `mod` length lst)

(++!) :: (Show a) => String -> a -> String
(++!) str = (str++) . show

clamp :: (Ord a) => a -> (a, a) -> a 
clamp var (low, high) = min (max var low) high

floatVertex :: (GLfloat,GLfloat,GLfloat) -> Vertex3 GLdouble
floatVertex tup = uncurry3 Vertex3 (mapT3 toFloating tup)

floatVector :: (GLfloat,GLfloat,GLfloat) -> Vector3 GLdouble
floatVector tup = uncurry3 Vector3 (mapT3 toFloating tup)

-- Maps a function across a list, except this function
-- can also be given a state variable like how foldl
-- works
mapWith :: (s -> a -> (b,s)) -> s -> [a] -> ([b], s)
mapWith func state (x:xs) = 
    let (x',s') = func state x in
    let (l,s) = mapWith func s' xs in (x':l, s)

mapWith _ s [] = ([],s)

{- Useful function that accepts two functions
 - and applies the third argument to both. Useful for
 - building up data flows with the same argument. Such
 - as:
 -
 - (bVertex3 >&> bNormal3) (0,0,1)
 - vs
 - bVertex3 (0,0,1) >> bNormal3 (0,0,1)
 -}
(>&>) :: (Monad m) => (a -> m b) -> (a -> m c) -> a -> m c
(>&>) f1 f2 a = f1 a >> f2 a

{- Instance where a monad can deconstruct
 - when the operation has failed -}
class (Monad m) => MonadHasFailure m where
    isFail :: m a -> Bool

instance MonadHasFailure Maybe where
    isFail = isNothing

instance MonadHasFailure [] where
    isFail = null

instance MonadHasFailure (Either a) where
    isFail (Left _) = True
    isFail _ = False


{- A way of chaining together commands such
 - that the first function in the chain that
 - returns a non-failing result is the one
 - that returns the result
 -
 - This is similar to the double pipe (||) operator
 - in imperative languages but with monads instead of
 - booleans.
 -}
(>|>) :: (MonadHasFailure m) => (a -> m c) -> (a -> m c) -> a -> m c
(>|>) f1 f2 a =
    let res = f1 a in
    isFail res ? f2 a $ res

(>||>) :: (MonadHasFailure m) => m a -> m a -> m a
(>||>) a b
        | isFail a = b
        | otherwise = a

whileM_ :: (Monad m) => (a -> Bool) -> m a -> a -> m a
whileM_ func routine start = do
    case func start of
        True -> routine >>= whileM_ func routine
        False -> return start

whileM :: (Monad m) => (a -> Bool) -> m a -> a -> m [a]
whileM bool routine' start' =
    whileM' bool routine' start' []
    where
        whileM' func routine start lst = do
            case func start of
                True -> do
                    next <- routine 
                    whileM' func routine next (lst ++ [start])
                False -> return lst

untilM_ :: (Monad m) => (a -> Bool) -> m a -> m a
untilM_ func routine = do
    start <- routine
    case func start of
        True -> untilM_ func routine
        False -> return start

untilM :: (Monad m) => (a -> Bool) -> m a -> m [a]
untilM func' routine' =
    untilM' func' routine' []
    where untilM' func routine lst = do
            start <- routine
            case func start of
                True -> untilM' func routine (lst ++ [start])
                False -> return lst

dFold :: [a] -> b -> (a -> a -> b -> b) -> b
dFold (x1:x2:xs) next func = dFold (x2:xs) (func x1 x2 next) func
dFold _ next _ = next

(!>>) :: a -> (a -> b) -> b
(!>>) a f = a `seq` f a

(!>>=) :: Monad m => m a -> (a -> m b) -> m b
(!>>=) a f = a !>> (flip (>>=) f)

{- Objective function composition. Useful to say
 - (drawArrays <..> numInstances) obj
 -}
(<..>) :: (b -> a -> c) -> (a -> b) -> a -> c
(<..>) f1 f2 a = f1 (f2 a) a

toHex :: (Integral a,Show a) => a -> String
toHex n | n == 0 = ""
        | otherwise = 
            let (quot',rem') = n `divMod` 16 in
            toHex quot' ++ [(index' !! fromIntegral rem')]
        where index' = "0123456789ABCDEFGHIJKlMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz"

average :: (Fold.Foldable a, Real c, Fractional b) => a c -> b
average lst =
    let (sum',count) = Fold.foldl' (\(sum_,count_) x -> (sum_ + x, count_ + 1)) (0,0) lst in
        (realToFrac sum') / count

maybeDefault :: a -> Maybe a -> a
maybeDefault a b = fromJust $ b >||> Just a

maybeDefaultM :: (Monad m) => Maybe a -> (a -> m ()) -> m () -> m ()
maybeDefaultM Nothing _ a = a
maybeDefaultM (Just a) b _ = b a

data MonadPlusBuilder a b  = MonadPlusBuilder a b

plusM :: a -> MonadPlusBuilder a ()
plusM a = MonadPlusBuilder a ()

runMonadPlusBuilder :: MonadPlusBuilder a b -> a
runMonadPlusBuilder (MonadPlusBuilder !a _) = a

instance (MonadPlus a) => Monad (MonadPlusBuilder (a b)) where
    return x = MonadPlusBuilder mzero x
    MonadPlusBuilder a1 _ >> MonadPlusBuilder a2 b = MonadPlusBuilder (a1 `mplus` a2) b
    builder@(MonadPlusBuilder _ b) >>= f = builder >> f b
    fail = undefined

untilM2 :: (Monad m) => (a -> m Bool) -> a -> (a -> m a) -> m a
untilM2 cond ini bod = do
    bool <- cond ini 
    if bool then return ini
        else  bod ini >>= \newini -> untilM2 cond newini bod

(<!>) :: (MArray a e IO, Ix i) => a i e -> i -> StateVar e
(<!>) arr idx = 
    let setter = writeArray arr idx
        getter = readArray arr idx in
        makeStateVar getter setter

for :: [a] -> (a -> b) -> [b]
for = flip map
