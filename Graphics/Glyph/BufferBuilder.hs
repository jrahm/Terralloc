{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Graphics.Glyph.BufferBuilder where

import Control.Monad
import Graphics.Rendering.OpenGL
import Foreign.Storable
import Foreign.Ptr
import Foreign.Marshal.Array
import Data.Array.Storable
import Data.Setters
import Debug.Trace
import qualified Data.Foldable as Fold
import Data.Sequence as Seq
import Data.Map as Map

import Graphics.Glyph.Mat4
import Graphics.Glyph.Util
import Graphics.Glyph.GLMath

import System.IO.Unsafe
import Unsafe.Coerce

data BufferBuilder3D = Plot BufferBuilder3D (GLfloat,GLfloat,GLfloat) Int Int | End
bufferSize :: BufferBuilder3D -> Int
bufferSize End = 0
bufferSize (Plot _ _ l _) = l

nelem :: BufferBuilder3D -> Int
nelem End = 0
nelem (Plot _ _ _ l) = l

sizeofGLfloat :: Int
sizeofGLfloat = 4

class Monad a => IsModelBuilder b a where
    plotVertex3 :: b -> b -> b -> a ()
    plotNormal  :: b -> b -> b -> a ()
    plotTexture :: b -> b ->a ()

{- A state monad that keeps track of operations
 - and will compile them into a buffer -}

data BuilderM b a = BuilderM (Builder (BuildDatum b)) a
data Builder b = Builder
                    !(Builder b) -- before
                    !(Builder b) -- after
                  | LeafBuilder !(Seq b) deriving Show

instance IsModelBuilder GLfloat (BuilderM GLfloat) where
    plotVertex3 x y z = bVertex3 (x,y,z)
    plotNormal x y z  = bNormal3 (x,y,z)
    plotTexture x y   = bTexture2 (x,y)

data BuildDatum b =
    VertexLink (b,b,b)  |
    NormalLink (b,b,b)  |
    ColorLink (b,b,b,b) |
    TextureLink (b,b) deriving Show

data CompiledBuild b = CompiledBuild {
    bStride :: Int,
    bEnabled :: (Bool,Bool,Bool),
    nElems  :: Int,
    array :: Ptr b,
    arrayBytes :: Int
}

bufferLength :: (Integral a) => CompiledBuild b -> a
bufferLength = fromIntegral . nElems

instance Show (CompiledBuild x) where
    show (CompiledBuild stride enabled n ptr nbytes) =
        "[CompiledBuild stride="++!stride++" enabled"++!enabled++" n="++!n++" ptr="++!ptr++" nbytes="++!nbytes++"]"

instance (Num t) => Monad (BuilderM t) where
    (BuilderM !builder1 _) >> (BuilderM !builder2 ret) =
        BuilderM (builder1 ><> builder2) ret
        where
            b1@(LeafBuilder !seq1) ><> b2@(LeafBuilder !seq2) 
                | Seq.length seq1 + Seq.length seq2 < 128 = LeafBuilder (seq1 >< seq2)
                | otherwise = Builder b1 b2
            (Builder !b1 !b2)  ><> leaf@(LeafBuilder !_)  =
                (Builder b1 (b2 ><> leaf))
            builder1 ><> builder2 = (Builder builder1 builder2)

    b1@(BuilderM _ ret) >>= func = b1 >> func ret

    return = BuilderM (LeafBuilder Seq.empty)
    fail = undefined

instance Functor Builder where
    fmap f (Builder b1 b2) = (Builder (fmap f b1) (fmap f b2))
    fmap f (LeafBuilder seq) = (LeafBuilder (fmap f seq))

instance Fold.Foldable Builder where
    foldl f ini (Builder b1 b2) = 
        Fold.foldl f (Fold.foldl f ini b1) b2
    foldl f ini (LeafBuilder seq) =
        Fold.foldl f ini seq

    foldr f ini (Builder b1 b2) = 
        Fold.foldr f (Fold.foldr f ini b2) b1
    foldr f ini (LeafBuilder seq) =
        Fold.foldr f ini seq

expandBuilder :: Builder a -> b -> (b -> a -> (b,[a])) -> Builder a
expandBuilder builder ini f = snd $ expandBuilder' builder ini f
    where expandBuilder' :: Builder a -> b -> (b -> a -> (b,[a])) -> (b,Builder a)

          expandBuilder' (Builder builder1 builder2) ini f = 
            let (snowball1,newBuilder1) = expandBuilder' builder1 ini f
                (snowball2,newBuilder2) = expandBuilder' builder2 snowball1 f in
                (snowball2,Builder newBuilder1 newBuilder2)

          expandBuilder' (LeafBuilder seq1) ini f = 
            let (seq,snow) = Fold.foldl' (\(seq', snow) datum -> 
                            let (snow',lst) = f snow datum in
                            (seq' >< Seq.fromList lst,snow')) (Seq.empty,ini) seq1 in
                (snow,LeafBuilder seq)

{- Add a vertex to the current builder -}
bVertex3 :: (a,a,a) -> BuilderM a ()
bVertex3 vert = BuilderM (LeafBuilder (Seq.singleton $ VertexLink vert)) ()

bTexture2 :: (a,a) -> BuilderM a ()
bTexture2 tex = BuilderM (LeafBuilder (Seq.singleton $ TextureLink tex)) ()

bNormal3 :: (a,a,a) -> BuilderM a ()
bNormal3 norm = BuilderM (LeafBuilder (Seq.singleton $ NormalLink norm)) ()

bColor4 :: (a,a,a,a) -> BuilderM a ()
bColor4 col = BuilderM (LeafBuilder (Seq.singleton $ ColorLink col)) ()

writeAndAvance :: (Storable a) => [a] -> Ptr a -> IO (Ptr a)
writeAndAvance (a:as) ptr = poke ptr a >> writeAndAvance as (advancePtr ptr 1)
writeAndAvance [] ptr = return ptr

compilingBuilder :: (Storable b, Num b, Show b) => BuilderM b x -> IO (CompiledBuild b)
compilingBuilder (BuilderM builder _) = do

    putStrLn "COMPILING"
    -- Size of the elements TODO unhardcode this
    let sizeof = sizeOf $ builderElem builder
                  where builderElem :: Builder (BuildDatum a) -> a
                        builderElem _ = unsafeCoerce (0::Int)

    {- Simply figure out what types of elementse
     - exist in this buffer -}
    let (bn,bc,bt,nVerts) = Fold.foldl' (\(bn,bc,bt,len) ele ->
                                case ele of
                                    NormalLink _ -> (True,bc,bt,len)
                                    ColorLink _ -> (bn,True,bt,len)
                                    TextureLink _ -> (bn,bc,True,len)
                                    VertexLink _ -> (bn,bc,bt,len+1)) (False,False,False,0) builder
    {- Calculate the stride; number of floats per element -}
    let stride = (3 + (?)bn * 3 + (?)bc * 4 + (?)bt * 2) * sizeof
                    where (?) True = 1
                          (?) False = 0

    let nbytes = stride * nVerts
    putStrLn $ "Mallocing array of size: " ++! nbytes
    array <- mallocArray nbytes

    -- Tuple
    -- Pointer to current element, current normal/color/texture
    putStrLn "Writing array buffer"
    !_ <- Fold.foldlM (\(ptr, cn, cc, ct) ele ->
           -- trace ("foldl " ++! ele) $
            case ele of
                NormalLink nn -> return (ptr,nn,cc,ct)
                ColorLink nc -> return (ptr,cn,nc,ct)
                TextureLink nt -> return (ptr,cn,cc,nt)
                VertexLink vert -> do
                  ptr' <- writeAndAvance (tp3 True vert) ptr >>=
                            writeAndAvance (tp3 bn cn) >>=
                            writeAndAvance (tp4 bc cc) >>=
                            writeAndAvance (tp2 bt ct)
                  return (ptr',cn,cc,ct) ) ( array, (0,0,0), (0,0,0,0), (0,0) ) builder
    putStrLn "Buffer written"
    let !compiledRet = CompiledBuild stride (bn,bc,bt) nVerts array nbytes
    putStrLn $ "COMPILE COMPLETE" ++! compiledRet
    return compiledRet

  where
      tp2 True (a,b)  = [a,b]
      tp2 False _  = []
 
      tp3 True (a,b,c) = [a,b,c]
      tp3 False _ = []
 
      tp4 True (a,b,c,d) = [a,b,c,d]
      tp4 False _ = []

storableArrayToBuffer :: (Storable el) => BufferTarget -> StorableArray Int el -> IO BufferObject
storableArrayToBuffer target arr = do
    let sizeof = sizeOf $ unsafePerformIO (readArray arr 0)
    [buffer] <- genObjectNames 1
    bindBuffer target $= Just buffer
    len <- getBounds arr >>= (\(a,b) -> return $ (b - a) * sizeof )
    withStorableArray arr $ \ptr ->
        bufferData target $= (fromIntegral len, ptr, StaticDraw)
    return buffer

ptrToBuffer :: (Storable b) => BufferTarget -> Int -> Ptr b -> IO BufferObject
ptrToBuffer target len ptr = do
    -- len is length in bytes
    [buffer] <- genObjectNames 1
    bindBuffer target $= Just buffer
    bufferData target $= (fromIntegral len, ptr, StaticDraw)
    return buffer

vertexArrayDescriptor :: CompiledBuild GLfloat -> VertexArrayDescriptor GLfloat
vertexArrayDescriptor (CompiledBuild stride _ _ _ _) = VertexArrayDescriptor 3 Float (fromIntegral stride) (wordPtrToPtr 0)

normalArrayDescriptor :: CompiledBuild GLfloat -> Maybe (VertexArrayDescriptor GLfloat)
normalArrayDescriptor (CompiledBuild stride (True,_,_) _ _ _) =
                Just $ VertexArrayDescriptor 3 Float
                        (fromIntegral stride) (wordPtrToPtr (3*4))
normalArrayDescriptor  _ = Nothing

colorArrayDescriptor :: CompiledBuild GLfloat -> Maybe (VertexArrayDescriptor GLfloat)
colorArrayDescriptor (CompiledBuild stride tup@(_,True,_) _ _ _) =
                Just $ VertexArrayDescriptor 4 Float
                        (fromIntegral stride) (wordPtrToPtr (offset tup))
                where offset (b1,_,_) = if b1 then (6*4) else (3*4)
                        
colorArrayDescriptor  _ = Nothing

textureArrayDescriptor :: CompiledBuild GLfloat -> Maybe (VertexArrayDescriptor GLfloat)
textureArrayDescriptor (CompiledBuild stride tup@(_,_,True) _ _ _) =
                Just $ VertexArrayDescriptor 2 Float
                        (fromIntegral stride) (wordPtrToPtr (offset tup))
                where offset (b1,b2,_) = (3 + (ifp b1 3) + (ifp b2 4)) * 4
                      ifp b x = if b then x else 0  
textureArrayDescriptor  _ = Nothing
createBufferObject :: BufferTarget -> CompiledBuild GLfloat -> IO BufferObject
createBufferObject target (CompiledBuild _ _ _ arr len) = ptrToBuffer target len arr

mapListInsert :: (Ord k) => k -> a -> Map.Map k [a] -> Map.Map k [a]
mapListInsert key val map =
    flip (Map.insert key) map $
        case Map.lookup key map of
            Nothing -> [val]
            Just x -> (val:x)

inferingNormals :: (RealFloat a,Ord a,Show a) => BuilderM a b -> BuilderM a b
inferingNormals (BuilderM builder ret) =
    let (normalMap,_,_) = Fold.foldl' (\(newMap, v1, v2) datum ->
                            case datum of
                                VertexLink w ->
                                    case (v1,v2) of
                                        (Just u, Just v) -> 
                                            let (Vec3 normal) = (Vec3 u <-> Vec3 v) × (Vec3 u <-> Vec3 w) in
                                            (insertWith (zipWithT3 (+)) w normal newMap, Nothing, Nothing)
                                        (Just u, Nothing) -> (newMap, v1, Just w)
                                        (Nothing,Nothing) -> (newMap, Just w, Nothing)
                                _ -> (newMap,v1,v2)
                                ) (Map.empty,Nothing,Nothing) builder in

    let newBuilder = expandBuilder builder () $ \() datum ->
         case datum of
            VertexLink tup ->
                let normalLink = NormalLink $ maybe (0,0,0) id $ Map.lookup tup normalMap in
                ((),[normalLink, datum])
            _ -> ((),[datum]) in

    (BuilderM newBuilder ret)


trianglesFromQuads :: [a] -> [a]
trianglesFromQuads (a:b:c:d:xs) = [a,b,c,a,c,d] ++ trianglesFromQuads xs
trianglesFromQuads l = l

translating :: (Num a) => (a,a,a) -> BuilderM a b -> BuilderM a b
translating trans (BuilderM builder ret) = do
    BuilderM (flip fmap builder $ \datum ->
                case datum of
                    VertexLink tup -> VertexLink $ zipWithT3 (+) tup trans
                    _ -> datum) ret
translating _ x = x
