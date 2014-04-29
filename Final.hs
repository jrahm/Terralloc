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

import qualified Data.Map as Map
import Data.Word
import Data.Array
import Data.Array.IO

import Data.Sequence as Seq
import Prelude as P

import Data.Bits

import Resources
import System.Random

import System.Environment


{-
 - This function builds an array of tile from the heightmap and
 - terrain map passed as SDL surfaces.
 -
 - Returns: An array with bounds [(0,0),(w,h)] of tiles where
 - w is the minimum width of the two images and h is the minimum
 - height.
 -}
buildArray :: SDL.Surface -> SDL.Surface -> Array (Int,Int) Tile
buildArray terrain height = 
    {- Pick the minimum width and height between the two images -}
    let w = min (SDL.surfaceGetWidth terrain) $ SDL.surfaceGetWidth height
        h = min (SDL.surfaceGetHeight terrain) $ SDL.surfaceGetHeight height

        {- Function that returns a Tile for an x y coordinate -}
        conv (x,y) = 
                let terrainVal = fromIntegral $ getPixelUnsafe x y terrain
                    {- The height is encoded as the sum of the color channels, to make life a litte
                     - easier on the heightmap reader. -}
                    sumit word =
                                ((word `shiftR` 8) .&. 0xFF) +
                                ((word `shiftR`16) .&. 0xFF) +
                                ((word `shiftR`24) .&. 0xFF)

                    {- The value of the hightmap at the coordinate. I will promise
                     - the compmiler that my surfaces will not change. -}
                    heightVal  = (fromIntegral.sumit) (getPixelUnsafe x y height)

                    {- The value of the terrain map at thata location -}
                    terrainVal' = Map.findWithDefault Resources.Unknown terrainVal tileMap in
                Tile terrainVal' heightVal

        {- build the list of Tiles to jam into the array -}
        list = map conv [(x,y) | x <- [0..w-1], y <- [0..h-1]]
        in listArray ((0,0),(w-1,h-1)) list

{- This function takes the array generated in the function from above and
 - creates a new array that colors in the array with locations of bodies
 - of water and assigns an id to each of them. This allows for me to go
 - back and assign heights for the bodies of water. -}
colorArray :: Array (Int,Int) Tile -> IO (IOArray (Int,Int) Int)
colorArray marr = do
    
    {- Very simple function that take splits a sequence
     - into a head and a tail -}
    let pollseq (Seq.viewl -> (head' :< tail')) = (head',tail')
        pollseq _ = undefined

    let bnd@(_,(w,h)) = bounds marr
    ret <- newArray bnd 0

    {- Boolean funcion. Returns true if the
     - tile at the position `place` is water
     - and has not yet been assigned an id -}
    let myfunction a_place = do
            val <- readArray ret a_place
            case marr ! a_place of
                (Tile Water _) -> return $ val==0
                _ -> return False

    {- Uses a queue method to flood fill bodies
     - of water and write that to an array -}
    let floodfill :: (Int,Int) -> ((Int,Int) -> IO Bool) -> Int -> IO ()
        floodfill start func' val = do
            let func t@(x,y) = if not (x <= w && x >= 0 && y <= h && y >= 0) then return False else func' t
            {- Just magic. Does a flood fill  -}
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
    {- Iterates through all the points and does a flood fill on
     - them -}
    foldM_ (\val place -> do
        bool <- myfunction place
        if bool then do
            floodfill place myfunction val
            return $ val+1
            else return val
        ) 1 [(x,y) | x <- [0..w], y <- [0..h]]
    return ret

{- This function takes the two arrays from the functions above and generates
 - 2 things:
 - A map of water bodies ids to elevations (to detect if you are under water
 - A builder that will generate all of the quads for the water. -}

getWaterQuads :: Array (Int,Int) Tile -> IOArray (Int,Int) Int -> IO ( Map.Map Int GLfloat, BuilderM GLfloat () )
getWaterQuads marr arr = do
    let (_,(w,h)) = bounds marr

    {- Iterates through the bodies of water and finds the lowest altitude
     - of the land surrounding the water. Returns a type of body id
     - to minx, miny, maxx, maxy and elevation -}
    let elevationCacheIO :: IO (Map.Map Int (Int,Int,Int,Int,Int))
        elevationCacheIO = do
            {- Tuple of functions that will be mapped with 
             - the application operator ($) -}
            let tup = (min,max,max,min,min)
            foldM (\themap (x,y) -> do
                    bodyID <- readArray arr (x,y)
                    if bodyID == 0 then return themap else do
                        let valid (aX,aY) = aX >= 0 && aX <= w && aY >= 0 && aY <= h
                        let neighbors (aX,aY) = P.filter valid $ map (zipWithT2 (+) (aX,aY))
                                                [      (1,0),       
                                                 (0,1),      (0,-1),
                                                       (-1,0)      ]
                        let toelev aX = 
                             let tile = marr ! aX in
                             (tileType tile == Water) ? 1000000000000 $ elevation tile
                        let elev = minimum $ map toelev (neighbors (x,y))
                        let newmap =
                             Map.insertWith (zipWithT5 (P.$) . zipWithT5 (P.$) tup)
                                bodyID (elev,x,y,x,y) themap
                        return newmap
                ) (Map.empty::Map.Map Int (Int,Int,Int,Int,Int)) [(x,y) | x <- [0..w], y <- [0..h]]

    elevMap <- elevationCacheIO  

    {- A map between body id and elevation. Get rid of the bounding quad box -}
    let elevMap2 = Map.map (\(elev,_,_,_,_) ->
                            fromIntegral elev / 10) elevMap

    let dat = Map.toList elevMap
    {- Iterate through the map and draw the bounding quad
     - for the body of water -}
    return (elevMap2,sequence_ $ for dat $ \(_, (elev,maxx,maxy,minx,miny)) ->
        let mxx = fromIntegral maxx + 1
            mnx = fromIntegral minx - 1
            mxy = fromIntegral maxy + 1
            mny = fromIntegral miny - 1
            relev = fromIntegral elev / 10 in
            mapM_ bVertex3
                [(mxx,relev,mxy),
                 (mxx,relev,mny),
                 (mnx,relev,mny),
                 (mnx,relev,mxy)])


printArray :: Array (Int,Int) Tile -> IO ()
printArray arr = do
    let (_,(w,h)) = bounds arr
    putStrLn $ "w=" ++! (w+1)
    putStrLn $ "h=" ++! (h+1)
    forM_ [0..h] $ \y -> do
        forM_ [0..w] $ \x -> do
            let lNext = arr ! (x,y)
            putStr $ show $ tileType lNext  
        putStr "    "
        forM_ [0..w] $ \x -> do
            let lNext = arr ! (x,y)
            putStr $ elevShow $ elevation lNext  
        putStrLn ""
    where elevShow x = 
            let len = P.length elevMap
                nx = x `div` 5 in
            if nx >= len then "=" else [elevMap !! nx]
          elevMap = "`.,-~*<:!;%&#@0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ"

printShowArray :: (Show a) => IOArray (Int,Int) a -> IO ()
printShowArray arr = do
    (_,(w,h)) <- getBounds arr
    putStrLn $ "w=" ++! (w+1)
    putStrLn $ "h=" ++! (h+1)
    forM_ [0..h] $ \y -> do
        forM_ [0..w] $ \x -> do
            lNext <- readArray arr (x,y)
            putStr $ show lNext 
        putStrLn ""

{- The colors each tile type is mapped to
 - as an array -}
toColor :: TileType -> (GLfloat,GLfloat,GLfloat,GLfloat)
toColor Tundra = (0.5,0.5,0.5,1.0)
toColor Mountains = (0.5,0.4,0.03,1.0)
toColor Grass = (0,0.3,0.0,1.0)
toColor Jungle = (0,1.0,0.0,1.0)
toColor Forest = (0,0.2,0.0,1.0)
toColor Beach = (0.7,0.7,0.6,1.0)
toColor Water = (0,0,1.0,1.0)
toColor Resources.Unknown = (0,0,0,0)

{- Map of color to TileType used for
 - parsing the terrain map -}
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

{- The function that generates the builder that will
 - generate the VAO for the terrain based on the heightmap -}
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

    inferingNormals $
        forM_ (trianglesFromQuads lst) $ \(x,y,z,_) -> do
            let f = fromIntegral

            {- Store the texture to use in the color -}
            let bUseTexture a = bColor4 (0,0,0,f a)

            bUseTexture $ fromEnum (tileType $ arr ! (x,z))
            bTexture2 (f x / 10.0, f z / 10.0)
            bVertex3 (f x, y,f z)
            
{- Generates random locations for the trees inside of the terrain
 - spots where trees may exist 
 -
 - A MonadPlusBuilder is a Monad used to build monad pluses; in this
 - case a Sequence.
 -}
createLocations :: Array (Int,Int) Tile -> StdGen -> Int -> TileType -> MonadPlusBuilder (Seq.Seq GLfloat) ()
createLocations arr gen density typ = do
    let (_,(w,h)) = bounds arr
    let getElev x y = if x >= w || y >= h || x < 0 || y < 0 then 0 else fromIntegral (elevation $ arr ! (x,y)) /10.0

    {- Adds a random number of trees between 0 and density for the location -}
    let run :: [Int] -> (Int,Int) -> MonadPlusBuilder ( Seq.Seq GLfloat ) [Int]
        run rs (x',y') = do
            let (_:ntrees, t) = P.splitAt (head rs `mod` density + 1) rs

            when (isType x' y' typ) $
                {- Iterate and place n trees -}
                forM_ ntrees $ \rand ->
                    let (a',b',c) = toTup rand
                        (x,y) = (int x' + f a', int y' + f b') :: (GLfloat,GLfloat)
                        [sx,sy,sz,rot,noise,shade] = (P.take 6 $ randomRs (0.0,1.0) $ mkStdGen c)

                        {- Boiler for finding the correct elevation between vertices -}
                        h1 = getElev (floor x) (floor y)
                        h2 = getElev (floor x) (floor (y+1))
                        h3 = getElev (floor (x+1)) (floor y)
                        h4 = getElev (floor (x+1)) (floor (y+1))
                        u = fpart x
                        v = fpart y
                        mixu1 = mix h3 h1 u
                        mixu2 = mix h4 h2 u
                        newh = mix mixu2 mixu1 v in

                        {- Add to the sequence of elements. This
                         - will be turned into a per-instance VAO -}
                        plusM $ Seq.fromList [
                                    -- translation
                                    x,newh-0.2,y,
                                    -- scale
                                    sx+0.5,sy+0.5,sz+0.5,
                                    -- rotation
                                    sin (rot*6.4), cos(rot*6.4),
                                    -- noise
                                    noise*6.4,
                                    shade / 2 + 0.75
                                ]

            {- Return the tail of the randomly generated numbers -}
            return t

    foldM_ run (randoms gen) [(x,y) | x <- [1..w], y <- [1..h]]

    return ()
    where isType x y t = tileType (arr ! (x,y)) == t
          f x = (fromIntegral x - 128) / 128 * (sqrt 2 / 2)
          toTup x = (  x .&. 0xFF ,
                      (x `shiftR` 8) .&. 0xFF,
                      (x `shiftR` 16) .&. 0xFF)
        

main :: IO ()
main = do
    let doload str = sequence
         [ SDLImg.load $ "maps/"++str++"_terrain.png",
           SDLImg.load $ "maps/"++str++"_height.png" ]
    args <- getArgs

    {- Load the terrain and heightmaps from SDL. -}
    [terrain,height] <- 
        case args of 
            (ter:hei:_) -> sequence [SDLImg.load ter, SDLImg.load hei]
            (m:_) -> doload m
            _ -> sequence [SDLImg.load "maps/wonderland_terrain.png", SDLImg.load "maps/wonderland_height.png"]

    let arr = buildArray terrain height
    coloredArr <- colorArray arr

    surface <- simpleStartup "Terralloc" (1280,1024)
    stgen <- newStdGen
    stgen2 <- newStdGen

    {- Create the tree locations. Desity of 7 for the forest, 2 for the jungle
     - since the jungle model is bigger -}
    let !forestLocations = runMonadPlusBuilder $ createLocations arr stgen 7 Forest
    let !jungleLocations = runMonadPlusBuilder $ createLocations arr stgen2 2 Jungle

    (mapping,water) <- getWaterQuads arr coloredArr
    coloredArr2 <- mapArray (\idx -> if idx == 0 then -1 else Map.findWithDefault (-1) idx mapping) coloredArr
    -- printShowArray coloredArr2

    {- Kick off SDL with the callbacks defined in Resources -}
    makeResources surface (createBuilder arr) forestLocations jungleLocations water arr coloredArr2
        >>= startPipeline reshape eventHandle displayHandle updateHandle;
