{-# LANGUAGE UndecidableInstances #-}
module Graphics.Glyph.ArrayGenerator where

import qualified Data.Map as M

import Data.Array
import Data.Maybe

data ArrayTransaction ix val b = ArrayBuilderM_ (M.Map ix val) b
instance (Ord ix) => Monad (ArrayTransaction ix a) where
    return = ArrayBuilderM_ M.empty
    (ArrayBuilderM_ map1 val) >>= f = 
        ArrayBuilderM_ (map1 `M.union` map2) val2
        where (ArrayBuilderM_ map2 val2) = f val

class HasDefault a where
    theDefault :: a

instance (Num a) => HasDefault a where
    theDefault = 0
instance (HasDefault a, HasDefault b) => HasDefault (a,b) where
    theDefault = (theDefault,theDefault)
instance (HasDefault a, HasDefault b, HasDefault c) => HasDefault (a,b,c) where
    theDefault = (theDefault,theDefault,theDefault)

writeArray :: ix -> a -> ArrayTransaction ix a ()
writeArray index' val = ArrayBuilderM_ (M.singleton index' val) ()

buildArray :: (Ix ix) => (ix,ix) -> e -> ArrayTransaction ix e () -> Array ix e
buildArray bounds' def (ArrayBuilderM_ map' _) =
    listArray bounds' [maybeLookup map' bound | bound <- range bounds'] 
    where maybeLookup map_ key = fromMaybe def (M.lookup key map_)

