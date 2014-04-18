{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
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

import Data.Sequence as Seq
import Prelude as P

import Debug.Trace
import Data.Bits

import TileShow
import Resources
import System.Random
import Debug.Trace



data TileType = Forest | Beach | Water | Grass | Jungle | Mountains | 
                Tundra | Unknown deriving (Enum,Eq)
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

-- colors regions of water in the array
colorArray :: Array (Int,Int) Tile -> IO (IOArray (Int,Int) Int)
colorArray marr = do
    let pollseq (Seq.viewl -> (head :< tail)) = (head,tail)
    let bnd@(_,(w,h)) = bounds marr
    ret <- newArray bnd 0
    let myfunction place = do
            val <- readArray ret place
            case marr ! place of
                (Tile Water _) -> return $ val==0
                _ -> return False
    let floodfill :: (Int,Int) -> ((Int,Int) -> IO Bool) -> Int -> IO ()
        floodfill start func' val = do
            let func t@(x,y) = if not (x <= w && x >= 0 && y <= h && y >= 0) then return False else func' t
            _ <- untilM2 (return . Seq.null) (Seq.singleton start) $ \queue -> do
                    let (head',tail') = pollseq queue
                    bool <- func head'
                    if not bool then return tail' else do
                        (_,tail2) <- untilM2 (liftM not . func . fst) (head',tail') $ \((x,y),queue') -> do
                            (ret <!> (x,y)) $= val
                            return ((x+1,y),queue' |> (x,y-1) |> (x,y+1))
                        (_,tail3) <- untilM2 (liftM not . func . fst) (head',tail2) $ \((x,y),queue') -> do
                            (ret <!> (x,y)) $= val
                            return ((x-1,y), queue' |> (x,y-1) |> (x,y+1))
                        return tail3
            return ()
    foldM_ (\val place -> do
        bool <- myfunction place
        if bool then do
            floodfill place myfunction val
            return $ val+1
            else return val
        ) 1 [(x,y) | x <- [0..w], y <- [0..h]]
    return ret

-- elevation quad is corner verticices
getWaterQuads :: Array (Int,Int) Tile -> IOArray (Int,Int) Int -> IO ( BuilderM GLfloat () )
getWaterQuads marr arr = do
    let (_,(w,h)) = bounds marr
    let elevationCacheIO :: IO (Map.Map Int (Int,Int,Int,Int,Int))
        elevationCacheIO = do
            let tup = (max,max,max,min,min)
            foldM (\themap (x,y) -> do
                    bodyID <- readArray arr (x,y)
                    if bodyID == 0 then return themap else do
                        let elev = elevation $ marr ! (x,y) :: Int
                        let newmap = Map.insertWith (\old->
                                            zipWithT5 (P.$) (zipWithT5 (P.$) tup old)
                                        ) bodyID (elev,x,y,x,y) themap
                        return newmap
                ) (Map.empty::Map.Map Int (Int,Int,Int,Int,Int)) [(x,y) | x <- [0..w], y <- [0..h]]

    dat <- (liftM Map.toList elevationCacheIO)
    return . sequence_ $ for dat $ \(_, (elev,maxx,maxy,minx,miny)) -> do
        let relev = (fromIntegral elev) / 10
            mxx = fromIntegral maxx
            mnx = fromIntegral minx
            mxy = fromIntegral maxy
            mny = fromIntegral miny
        mapM_ bVertex3 $ trianglesFromQuads
            [(mxx,relev,mxy),
             (mxx,relev,mny),
             (mnx,relev,mny),
             (mnx,relev,mxy)] 


printArray :: Array (Int,Int) Tile -> IO ()
printArray arr = do
    let (_,(w,h)) = bounds arr
    putStrLn $ "w=" ++! (w+1)
    putStrLn $ "h=" ++! (h+1)
    forM_ [0..h] $ \y -> do
        forM_ [0..w] $ \x -> do
            let next = arr ! (x,y)
            putStr $ (show $ tileType next) 
        putStr "    "
        forM_ [0..w] $ \x -> do
            let next = arr ! (x,y)
            putStr $ (elevShow $ elevation next) 
        putStrLn ""
    where elevShow x = 
            let len = P.length elevMap
                nx = x `div` 5 in
            if nx >= len then "=" else [elevMap !! nx]
          elevMap = "`.,-~*<:!;%&#@0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ"

printShowArray arr = do
    (_,(w,h)) <- getBounds arr
    putStrLn $ "w=" ++! (w+1)
    putStrLn $ "h=" ++! (h+1)
    forM_ [0..h] $ \y -> do
        forM_ [0..w] $ \x -> do
            next <- readArray arr (x,y)
            putStr $ (show $ next) 
        putStrLn ""

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
            
createLocations :: Array (Int,Int) Tile -> StdGen -> Int -> TileType -> MonadPlusBuilder (Seq.Seq GLfloat) ()
createLocations arr gen density typ = do
    let (_,(w,h)) = bounds arr
    let getElev x y = if x >= w || y >= h || x < 0 || y < 0 then 0 else fromIntegral (elevation $ arr ! (x,y)) /10.0

    let run :: [Int] -> (Int,Int) -> MonadPlusBuilder ( Seq.Seq GLfloat ) [Int]
        run rs (x,y) = do
            let ((_:he), t) = P.splitAt (head rs `mod` density + 1) rs
            let signum' = floor.signum

            when (isType x y typ) $ do
                forM_ he $ \rand -> do
                    let (a',b',c) = toTup rand
                    let (a,b) = (f a', f b')
                    let [sx,sy,sz,rot,noise] = (P.take 5 $ randomRs (0.0,1.0) $ mkStdGen c)

                    let elev = getElev x y
                    let elev_dx = getElev (x + signum' a) y
                    let elev_dy = getElev x (y + signum' b)
                    let realelev =
                         ((elev * (1-abs a) + elev_dx * (abs a)) +
                          (elev * (1-abs b) + elev_dy * (abs b))) / 2.0

                    when (elev_dx > 0 && elev_dy > 0) $
                            plusM $ Seq.fromList [
                                    -- translation
                                    fromIntegral x+a,realelev,fromIntegral y+b,
                                    -- scale
                                    sx+0.5,sy+0.5,sz+0.5,
                                    -- rotation
                                    sin (rot*6.4), cos(rot*6.4),
                                    -- noise
                                    noise*6.4
                                ]
            return t

    foldM_ run (randoms gen) [(x,y) | x <- [1..w], y <- [1..h]]

    return ()
    where isType x y t =
            (tileType $ arr ! (x,y)) == t
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
    printArray arr
    coloredArr <- colorArray arr
    printShowArray coloredArr

    surface <- simpleStartup "Spectical" (640,480)
    stgen <- newStdGen
    stgen2 <- newStdGen
--    (log',file) <- loadObjFile "tree.obj"
--    mapM_ putStrLn log'

    let !forestLocations = runMonadPlusBuilder $ createLocations arr stgen 7 Forest
    let !jungleLocations = runMonadPlusBuilder $ createLocations arr stgen2 2 Jungle

    water <- getWaterQuads arr coloredArr
--  putStrLn $ "ForestLocations :" ++! forestLocations
    makeResources surface (createBuilder arr) forestLocations jungleLocations water
        >>= startPipeline reshape eventHandle displayHandle updateHandle;
