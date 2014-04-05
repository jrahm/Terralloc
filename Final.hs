{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE BangPatterns #-}
module Main where

import Graphics.Rendering.OpenGL as GL
import Graphics.UI.SDL.Image as SDLImg
import Graphics.UI.SDL as SDL
import Graphics.SDL.SDLHelp
import Graphics.Glyph.Util
import Control.Monad

import Graphics.Glyph.BufferBuilder
import Graphics.Glyph.ObjLoader

import qualified Data.Map as Map
import Data.Word
import Data.Array
import Data.Array.IO

import Debug.Trace
import Data.Bits

import TileShow
import Resources
import System.Random
import Debug.Trace



data TileType = Forest | Beach | Water | Grass | Jungle | Mountains | 
                Tundra | Unknown deriving Enum
$(makeShow ''TileType)


data Tile = Tile {
    tileType :: TileType,
    elevation :: Int
} deriving Show

buildArray :: SDL.Surface -> SDL.Surface -> IO (Array (Int,Int) Tile)
buildArray terrain height = 
    let w = min (SDL.surfaceGetWidth terrain) $ SDL.surfaceGetWidth height
        h = min (SDL.surfaceGetHeight terrain) $ SDL.surfaceGetHeight height
        conv (x,y) = 
                let terrainVal = fromIntegral $ getPixelUnsafe x y terrain
                    sumit word =
                                ((word `shiftR` 8) .&. 0xFF) +
                                ((word `shiftR`16) .&. 0xFF) +
                                ((word `shiftR`24) .&. 0xFF)
                    heightVal  = (fromIntegral.sumit) (getPixelUnsafe x y height)
                    terrainVal' = Map.findWithDefault Main.Unknown terrainVal tileMap in
                Tile terrainVal' heightVal
        list = map conv [(x,y) | x <- [0..w-1], y <- [0..h-1]]

        in do
            putStrLn $ show (head list)
            return $ listArray ((0,0),(w-1,h-1)) list

printArray :: Array (Int,Int) Tile -> IO ()
printArray arr = do
    let (_,(w,h)) = bounds arr
    putStrLn $ "w=" ++! w
    putStrLn $ "h=" ++! h
    forM_ [0..h-1] $ \y -> do
        forM_ [0..w-1] $ \x -> do
            let next = arr ! (x,y)
            putStr $ (show $ tileType next) 
        putStr "    "
        forM_ [0..w-1] $ \x -> do
            let next = arr ! (x,y)
            putStr $ (elevShow $ elevation next) 
        putStrLn ""
    where elevShow x = 
            let len = length elevMap
                nx = x `div` 5 in
            if nx > len then "=" else [elevMap !! nx]
          elevMap = "`.,-~*<:!;%&#@0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ"

toColor :: TileType -> (GLfloat,GLfloat,GLfloat,GLfloat)
toColor Tundra = (0.5,0.5,0.5,1.0)
toColor Mountains = (0.5,0.4,0.03,1.0)
toColor Grass = (0,0.3,0.0,1.0)
toColor Jungle = (0,1.0,0.0,1.0)
toColor Forest = (0,0.2,0.0,1.0)
toColor Beach = (0.7,0.7,0.6,1.0)
toColor Water = (0,0,1.0,1.0)

tileMap :: Map.Map Word32 TileType
tileMap =
    let c = rgbToWord in
    Map.insert    (c 100 100 100) Tundra  $
    Map.insert    (c 128 100 20) Mountains  $
    Map.insert    (c 0 100 0) Grass  $
    Map.insert    (c 0 255 0) Jungle $
    Map.insert    (c 0 50 0) Forest $
    Map.insert    (c 255 255 255) Beach  $
    Map.singleton (c 0 0 255) Water

createBuilder :: Array (Int,Int) Tile -> BuilderM GLfloat ()
createBuilder arr = do
    let (_,(w,h)) = bounds arr

    let lst = concatMap (\(x,y) ->
            let g (x',z',w') = (x', fromIntegral (elevation $ arr ! (x',z')) / 10.0, z', w') in

            [g (x,  y  ,1::Int),
             g (x-1,y  ,1),
             g (x-1,y-1,1),
             g (x,  y-1,1)] )

            [(x,y) | x <- [1..w], y <- [1..h]]

    inferingNormals $ do
        forM_ (trianglesFromQuads lst) $ \(x,y,z,_) -> do
            let f = fromIntegral
            let bUseTexture a = bColor4 (0,0,0,f a)
            -- TODO un hardcode these
            bUseTexture $ fromEnum (tileType $ arr ! (x,z))
            bTexture2 (f x / 10.0, f z / 10.0)
            bVertex3 (f x, y,f z)
            
createForestBuilder :: Array (Int,Int) Tile -> StdGen -> ObjectFile GLfloat -> BuilderM GLfloat ()
createForestBuilder arr gen file = do

    let (_,(w,h)) = bounds arr
    let getElev x y = if x >= w || y >= h || x < 0 || y < 0 then 0 else fromIntegral (elevation $ arr ! (x,y)) /10.0
    let !treeF = 
         trace "build tree" $
            basicBuildObject file

    let run :: [Int] -> (Int,Int) -> BuilderM GLfloat [Int]
        run rs (x,y) = do
            let ((_:he), t) = splitAt (head rs `mod` 13 + 1) rs
            let signum' = floor.signum

            when (isForest x y) $ do
                forM_ he $ \rand -> do
                    let (a,b,_) = mapT3 f (toTup rand)
                    let elev = getElev x y
                    let elev_dx = getElev (x + signum' a) y
                    let elev_dy = getElev x (y + signum' b)
                    let realelev =
                         ((elev * (1-abs a) + elev_dx * (abs a)) +
                          (elev * (1-abs b) + elev_dy * (abs b))) / 2.0
                    when (elev_dx > 0 && elev_dy > 0) $ do
                        translating (fromIntegral x+a,realelev,fromIntegral y+b) $ do
                            treeF
                            

            return t

    _ <- foldM run (randoms gen) [(x,y) | x <- [1..w], y <- [1..h]]

    return ()
    where isForest x y =
            case tileType $ arr ! (x,y) of
                Forest -> True
                _ -> False
          f x = (fromIntegral x - 128) / 128 * (sqrt 2 / 2)
          toTup x = ( (x .&. 0xFF),
                      (x `shiftR` 8) .&. 0xFF,
                      (x `shiftR` 16) .&. 0xFF)
        

main :: IO ()
main = do
    putStrLn "Loading..."
    terrain <- SDLImg.load "terrain.png"
    height <- SDLImg.load "height.png"
    putStrLn "Done Loading ..."

    arr <- buildArray terrain height
    putStrLn "Array Built"
    -- printArray arr

    surface <- simpleStartup "Spectical" (640,480)
    stgen <- newStdGen
    (log',file) <- loadObjFile "tree.obj"
    mapM_ putStrLn log'

    makeResources surface (createBuilder arr) (createForestBuilder arr stgen file) >>= startPipeline reshape eventHandle displayHandle updateHandle;
