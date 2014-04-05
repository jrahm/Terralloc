module Graphics.Glyph.ObjLoader where

import Graphics.Glyph.BufferBuilder
import Graphics.Glyph.Util
import Debug.Trace

import Control.Monad
import Data.Either
import Data.String.Utils
import Data.Array
import System.IO
import qualified Data.Map as M

import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Lazy.Char8 as C

data ObjectFile a = ObjectFile [ObjectStatement a]

data ObjectStatement a =
    Nop |
    VertexStatement (a,a,a)       |
    TextureStatement (a,a)    |
    VertexNormalStatement (a,a,a) |
    UseMaterialStatement (String)           |
    MaterialLibraryStatement String         |
    FaceStatement [(Int,Int,Int)]   deriving Show

foldl2 :: a -> [b] -> (a -> b -> a) -> a
foldl2 a b c = foldl  c a b

isNop :: ObjectStatement a -> Bool
isNop x = case x of 
    Nop -> True
    _ -> False

isVertex :: ObjectStatement a -> Bool
isVertex (VertexStatement _) = True
isVertex _ = False

isNormal :: ObjectStatement a -> Bool
isNormal (VertexNormalStatement _) = True
isNormal _ = False

isTexture :: ObjectStatement a -> Bool
isTexture (TextureStatement _) = True
isTexture _ = False

basicBuildObject :: (Floating b, IsModelBuilder b a) => ObjectFile b -> a ()
basicBuildObject (ObjectFile list) =
    let fromList lst = listArray (0,length lst-1) lst in

    -- Set up the lists as arrays for fast access
    let vertexList  = fromList $ map (\stmt -> 
                        case stmt of
                            (VertexStatement v) -> v
                            _ -> (0,0,0)) (filter isVertex list) in

    let normalList  = fromList $ map (\stmt -> 
                        case stmt of
                            (VertexNormalStatement v) -> v
                            _ -> (0,0,0)) (filter isNormal list) in

    let textureList  = fromList $ map (\stmt -> 
                        case stmt of
                            (TextureStatement v) -> v
                            _ -> (0,0)) (filter isTexture list) in

    forM_ list $ \stmt ->
        case stmt of
            (FaceStatement arr) ->
                forM_ arr $ \(a,b,c) -> do
                    when (c >= 0) (uncurry3 plotNormal  $ normalList  ! (c-1))
                    when (b >= 0) (uncurry  plotTexture $ textureList ! (b-1))
                    when (a >= 0) (uncurry3 plotVertex3 $ vertexList  ! (a-1))
            _ -> return ()
                        

loadObjFromBytestring :: (Read b) => L.ByteString -> ([String], ObjectFile b)
loadObjFromBytestring _contents =
    let contents::[L.ByteString] ; contents = C.split '\n' _contents in
    let mys2n str = case str of
         "" -> -1
         _ -> read str in

    let s2t s = case split "/" s of
         [a,b,c] -> Just (mapT3 mys2n (a,b,c))
         [a,b] -> Just (mapT3 mys2n (a,b,""))
         [a] -> Just (mapT3 mys2n (a,"",""))
         _ -> Nothing in

    let compiled =
            map (\(num,line) -> case words $ C.unpack line of
    
            [] -> Right Nop -- This is an empty line
            (('#':_):_)  -> Right Nop -- This is a comment, so use a 'nop'
            ("o":_) -> Right Nop -- Not really of use
            
            ["v",x,y,z]  -> Right $ VertexStatement ( (read x), (read y), (read z))
            ["vt",x,y]   -> Right $ TextureStatement ( (read x), (read y))
            ["vn",x,y,z] -> Right $ VertexNormalStatement ( (read x), (read y), (read z))
            ["usemtl", mtl] -> Right $ UseMaterialStatement mtl
            ["mtllib", lib] -> Right $ MaterialLibraryStatement lib

            ("f":_tail) -> case mapM s2t _tail of
                Just lst -> Right $ FaceStatement lst
                _ -> Left $ foldl (++) "" ["Syntax error in face value on line ", show num, " `", C.unpack line, "'" ]

            _ -> Left $ foldl (++) "" ["Unrecognized Sequence on line ", show num, " `", C.unpack line, "'" ]

            ) (zip [(1::Int)..] contents) in

    ( lefts compiled, ObjectFile (filter (not.isNop) $ rights compiled) )
    

loadObjFromHandle :: (Read b) => Handle -> IO ([String], ObjectFile b)
loadObjFromHandle = loadObjFromHandleWithFilter id

loadObjFromHandleWithFilter :: (Read b) => (L.ByteString -> L.ByteString) -> Handle -> IO ([String], ObjectFile b)
loadObjFromHandleWithFilter _filter handle =
    liftM (loadObjFromBytestring . _filter) (L.hGetContents handle)

loadObjFile :: (Read b) => FilePath -> IO ([String], ObjectFile b)
loadObjFile = loadObjFileWithFilter id

loadObjFileWithFilter :: (Read b) => (L.ByteString -> L.ByteString) -> FilePath -> IO ([String], ObjectFile b)
loadObjFileWithFilter filt path = loadObjFromHandleWithFilter filt =<< openFile path ReadMode
