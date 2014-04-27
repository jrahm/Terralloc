{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE TemplateHaskell #-}
module Resources where

import Graphics.UI.SDL as SDL 
import Graphics.UI.SDL.Image as SDLImg

import Foreign.Storable
import Foreign.Ptr
import Foreign.Marshal.Array

import Graphics.Glyph.GLMath as V
import Graphics.Glyph.GlyphObject
import Graphics.Glyph.ObjLoader
import Graphics.Glyph.Shaders
import Graphics.SDL.SDLHelp
import Graphics.Glyph.BufferBuilder
import Graphics.Glyph.Mat4
import Graphics.Glyph.Util
import Graphics.Glyph.ExtendedGL as Ex
import Graphics.Rendering.OpenGL as GL
import Graphics.Rendering.OpenGL.Raw.Core31

import Control.Applicative
import Control.Monad

import Data.Angle
import Data.Function
import Data.Setters
import qualified Data.Sequence as Seq
import qualified Data.Foldable as Fold
import Data.Maybe
import Debug.Trace

import System.Exit
import qualified Data.Array.IO as ArrIO

import TileShow

import Data.Array
import qualified Data.StateVar as SV

{- Types of terrain which are possible -}
data TileType = Forest | Beach | Water | Grass | Jungle | Mountains | 
                Tundra | Unknown deriving (Enum,Eq)
$(makeShow ''TileType)

{- A tile has 2 things, a type and
 - elevation, however, the term tile is
 - a litte misleading, it is really a point. -}
data Tile = Tile {
    tileType :: TileType,
    elevation :: Int
} deriving Show

{- Position of the camera as described by
 - polar coordinates -}
data CameraPosition = CameraPosition {
    pEye :: Vec3 GLfloat,
    pTh :: GLfloat,
    pPh :: GLfloat
} deriving Show

{- The central data type for rendering
 - the scene. Contains the 'global' information -}
data Resources = Resources {
    rSurface :: SDL.Surface,

    rPosition :: CameraPosition,
    rDPosition :: CameraPosition,

    pMatrix :: Mat4 GLfloat,
    mvMatrix :: Mat4 GLfloat,

    routines :: [ResourcesClosure -> IO ()],

    speed :: GLfloat,
    timeSpeed :: Int,
    time :: Int,

    heightMap :: Array (Int,Int) Tile,
    positionUpdate :: (Resources -> IO Resources),
    speedFactor :: GLfloat,
    dDown :: GLfloat,
    waterArray :: ArrIO.IOArray (Int,Int) GLfloat
}

{- Central data type for rendering each frame -}
data ResourcesClosure = ResourcesClosure {
      rcMVMatrix :: Mat4 GLfloat
    , rcPMatrix  :: Mat4 GLfloat
    , rcLightPos :: Vec4 GLfloat
    , rcTime     :: GLfloat
    , rcNormalMatrix  :: Mat3 GLfloat
    , rcGlobalAmbient :: Vec4 GLfloat
    , rcCameraPos :: CameraPosition
    , rcCameraLocation :: Vec3 GLfloat
    , rcResources :: Resources
}

$(declareSetters ''Resources)

{- A function that makes the resources data first
 - person -}
firstPerson :: Resources -> IO Resources
firstPerson res =
    let (CameraPosition (Vec3 (x,curh,y)) th ph) = rPosition res
        (_,(w,h)) = bounds $ heightMap res
        (!!!) arr (x',y') = if x' < 0 || y' < 0 || x' > w || y' > h then -1000 else elevation (arr ! (x',y'))
        h1 = ((/10.0).fromIntegral) (heightMap res !!! (floor x, floor y) )
        h2 = ((/10.0).fromIntegral) (heightMap res !!! (floor x, floor (y+1)) )
        h3 = ((/10.0).fromIntegral) (heightMap res !!! (floor (x+1), floor y) )
        h4 = ((/10.0).fromIntegral) (heightMap res !!! (floor (x+1), floor (y+1)))
        u = x - (int $ (floor x::Int))
        v = y - (int $ (floor y::Int))
        mixu1 = mix h3 h1 u
        mixu2 = mix h4 h2 u
        newh = mix mixu2 mixu1 v + 0.2
        droph = curh - dDown res
        in do
    return $ 
        if (newh+0.2 > droph) then
            setRPosition (CameraPosition (Vec3 (x,newh,y)) th ph) $
                    setDDown 0 $
                        if speed res > speedFactor res then
                            (setSpeed <..> speedFactor) res
                            else res
            else
                setRPosition (CameraPosition (Vec3 (x, droph, y)) th ph) $
                        setDDown (dDown res + 0.05) res

{- A function which will explode if a uniform
 - does not exist for the shader given, otherwis,
 - it will return a list of uniform locations -}
getUniformsSafe :: Program -> [String] -> IO [UniformLocation]
getUniformsSafe prog uniforms =
    forM uniforms $ \a_uniform -> do
        tmp <- get $ uniformLocation prog a_uniform
        case tmp of
            UniformLocation (-1) -> do
                putStrLn $ "No uniform with name: "++a_uniform
                exitWith (ExitFailure 112)
            _ ->  return tmp

{- Builds an model view matrix given the
 - camera position of the scene -}
buildMVMatrix :: CameraPosition -> Mat4 GLfloat
buildMVMatrix (CameraPosition eye th ph) =
    let up = if ph' >= 90 && ph' < 270 then Vec3 (0,-1,0) else Vec3 (0,1,0)
            where ph' = (floor ph::Int) `mod` 360 in
    let lookat = eye <+> (Vec3 $ toEuclidian (1,th,ph)) in
        lookAtMatrix eye lookat up

{- Called after each frame to crunch throught the
 - events -}
eventHandle :: SDL.Event -> Resources -> IO Resources
eventHandle event res = do
    let (CameraPosition eye th ph) = rDPosition res
    let (CameraPosition peye pth pph) = rPosition res
    case event of
        KeyDown (Keysym SDLK_ESCAPE _ _) -> exitSuccess

        KeyDown (Keysym SDLK_EQUALS _ _) ->
            return $ (setTimeSpeed <..> ((+1).timeSpeed)) res
        KeyDown (Keysym SDLK_MINUS _ _) ->
            return $ (setTimeSpeed <..> ((subtract 1).timeSpeed)) res

        KeyDown (Keysym SDLK_UP _ _) ->
            return $ setRDPosition (CameraPosition eye th (ph+1)) res
        KeyDown (Keysym SDLK_DOWN _ _) ->
            return $ setRDPosition (CameraPosition eye th (ph-1)) res
        KeyDown (Keysym SDLK_RIGHT _ _) ->
            return $ setRDPosition (CameraPosition eye (th+1) ph) res
        KeyDown (Keysym SDLK_LEFT _ _) ->
            return $ setRDPosition (CameraPosition eye (th-1) ph) res

        KeyUp (Keysym SDLK_UP _ _) ->
            return $ setRDPosition (CameraPosition eye th (ph-1)) res
        KeyUp (Keysym SDLK_DOWN _ _) ->
            return $ setRDPosition (CameraPosition eye th (ph+1)) res
        KeyUp (Keysym SDLK_RIGHT _ _) ->
            return $ setRDPosition (CameraPosition eye (th-1) ph) res
        KeyUp (Keysym SDLK_LEFT _ _) ->
            return $ setRDPosition (CameraPosition eye (th+1) ph) res

        MouseMotion _ _ x y -> do
            return $ setRPosition (CameraPosition peye (pth+(fromIntegral x/30.0)) (pph-(fromIntegral y/30.0))) res

        KeyDown (Keysym SDLK_w _ _) ->
            return $ setSpeed (speed res + speedFactor res) res
        KeyDown (Keysym SDLK_s _ _) ->
            return $ setSpeed (speed res - speedFactor res) res
        KeyUp (Keysym SDLK_w _ _) ->
            return $ setSpeed 0 res
        KeyUp (Keysym SDLK_s _ _) ->
            return $ setSpeed 0 res

        KeyUp (Keysym SDLK_q _ _) ->
            let getY (Vec3 (_,y,_)) = y in
            return $
                setPositionUpdate firstPerson $
                setSpeedFactor 0.1 $
                    (setDDown <..> (negate . getY . resourcesVelocity)) res
        KeyUp (Keysym SDLK_e _ _) ->
            return $
                setPositionUpdate return $
                setSpeedFactor 1 $
                    if speed res > 0 then setSpeed 1 res else res

        KeyUp (Keysym SDLK_f _ _) -> do
            ret <- reshape 1920 1080 res
            SDL.toggleFullscreen $ rSurface ret
            SDL.showCursor False
            SDL.grabInput True
            return ret
        KeyUp (Keysym SDLK_g _ _) -> do
            SDL.showCursor False
            SDL.grabInput True
            return res

        KeyDown (Keysym SDLK_SPACE _ _) -> do
            return $ setDDown (-0.3) res

        KeyDown (Keysym SDLK_LSHIFT _ _) -> do
            return $ (setSpeed <..> ((*3) . speed)) res
        KeyUp (Keysym SDLK_LSHIFT _ _) -> do
            return $ (setSpeed <..> ((/3) . speed)) res

        _ -> return res

{- Callback for the display -}
displayHandle :: Resources -> IO Resources
displayHandle resources = do
    let cameraPos@(CameraPosition loc _ _) = rPosition resources
    let lighty = ((/10) . fromIntegral . time) resources
    let logist c =  (1 / (1 + 2.71828**(-c*x))) * 0.9 + 0.1
            where x = sine $ Degrees (lighty)
    let globalAmbient::(GLfloat,GLfloat,GLfloat,GLfloat)
        globalAmbient = ( logist 2+0.1, logist 10, (logist 15) + 0.1,(sine.Degrees) lighty)
    let lightPos = Vec4( 50,
                         1000000 * (sine.Degrees $ lighty),
                         -1000000 * (cosine.Degrees . (/10) . fromIntegral . time) resources,
                         1 )
    let l_mvMatrix = buildMVMatrix $ cameraPos
    let normalMatrix = glslModelViewToNormalMatrix l_mvMatrix

    clearColor $= Color4 0 0 0 0
    clear [ColorBuffer, DepthBuffer]
    SDL.flip $ rSurface resources
    printErrors "Display"


    let rc = ResourcesClosure l_mvMatrix
                (pMatrix resources)
                (l_mvMatrix `glslMatMul` lightPos)
                (fromIntegral $ time resources)
                (normalMatrix)
                (Vec4 globalAmbient)
                cameraPos
                loc
                resources

                in mapM_ (Prelude.$rc) $ routines resources

    SDL.glSwapBuffers
    return resources

cameraToEuclidian :: CameraPosition -> Vec3 GLfloat
cameraToEuclidian (CameraPosition _ ph th) = V.normalize $ Vec3 $ toEuclidian (1,ph,th)

resourcesVelocity :: Resources -> Vec3 GLfloat
resourcesVelocity res = speed res `vScale` cameraToEuclidian (rPosition res)

resourcesUnderWater :: Resources -> IO Bool
resourcesUnderWater res = do
    let (CameraPosition (Vec3 (x,ch,y)) _ _) = rPosition res
    (_,(w,h)) <- ArrIO.getBounds $ waterArray res
    if x < 0 || y < 0 || x > int w || y > int h then return False else do
        height <- ArrIO.readArray (waterArray res) (floor x, floor y)
        return (height > ch && height >= 0)

updateHandle :: Resources -> IO Resources
updateHandle res = do
   (positionUpdate res) $ setRPosition (rPosition res `cAdd` rDPosition res) $
            let new = ((+) `on` (Prelude.$ res)) timeSpeed time  in
                setTime new res
   where (CameraPosition x y z) `cAdd` (CameraPosition _ y' z') =
           let x' = speed res `vScale` (V.normalize $ Vec3 $ toEuclidian (1,y, z)) in
            (CameraPosition (x <+> x') (y + y') (z + z'))

reshape :: Int -> Int -> Resources -> IO Resources
reshape w h res =
    defaultReshape w h () >> do
        let pMatrix' = perspectiveMatrix 50 (fromIntegral w / fromIntegral h) 0.1 10000
        return $ setPMatrix pMatrix' res

loadProgramSafe' ::
    (IsShaderSource a,
     IsShaderSource b,
     IsShaderSource c) => a -> b -> Maybe c -> IO Program
loadProgramSafe' s1 s2 s3 = do
    progMaybe <- loadProgramSafe s1 s2 s3
    when (isNothing progMaybe) $ exitWith (ExitFailure 111)
    return $ fromJust progMaybe

loadProgramFullSafe' :: 
    (IsShaderSource tc, IsShaderSource te,
     IsShaderSource g, IsShaderSource v,
     IsShaderSource f) => Maybe (tc, te) -> Maybe g -> v -> f -> IO Program
loadProgramFullSafe' a b c d = do
    progMaybe <- loadProgramFullSafe a b c d
    when (isNothing progMaybe) $ exitWith (ExitFailure 111)
    return $ fromJust progMaybe

buildTerrainObject :: BuilderM GLfloat b -> IO (ResourcesClosure -> IO ())
buildTerrainObject builder = do
    let terrainList = map ("terrain/"++)
         [ "forest.png", "beach.png",
           "oceanfloor.png", "grass.png",
           "jungle.png", "mountains.png",
           "tundra.png" ]
    print terrainList
    terrainProg <- loadProgramSafe' "shaders/basic.vert" "shaders/basic.frag" (Nothing::Maybe String)
    lst <- forM (zip [0..7::Int] $ terrainList ++ repeat "terrain/unknown.png") $ \(idx,str) -> do
            location <- get $ uniformLocation terrainProg $ "textures[" ++! idx ++ "]"
            load str >>= textureFromSurface >>= return . (,) location

    let (dx,dy) = (mapT2 $ (1/).fromIntegral) (mapT2 maximum (unzip $ map (textureSize.snd) lst));
    dXlocation <- get $ uniformLocation terrainProg "dX"
    dYlocation <- get $ uniformLocation terrainProg "dY"
    putStrLn $ "(dx,dy)=" ++! (dx,dy)
    obj <- newDefaultGlyphObjectWithClosure builder () $ \_ -> do
                    currentProgram $= Just terrainProg
                    forM_ (zip [0..] lst) $ \(i,(loc,td)) ->
                        setupTexturing td loc i
                    uniform dXlocation $= Index1 (dx::GLfloat)
                    uniform dYlocation $= Index1 (dy::GLfloat)
                    printErrors "terrainObjectClosure"

    [lightposU, globalAmbientU, pjMatrixU, mvMatrixU, normalMatrixU, fogU]
        <- getUniformsSafe terrainProg ["lightPos","globalAmbient","pjMatrix","mvMatrix","normalMatrix","fog"]
    return $ \rc -> do
        draw $ prepare obj $ \_ -> do
            cullFace $= Just Front
            uniform mvMatrixU $= rcMVMatrix rc
            uniform pjMatrixU $= rcPMatrix rc
            uniform lightposU $= rcLightPos rc
            uniform normalMatrixU $= rcNormalMatrix rc
            uniform globalAmbientU $= rcGlobalAmbient rc
            bool <- (resourcesUnderWater $ rcResources rc)
            if bool then
                uniform fogU $= Index1 (0.9::GLfloat) else
                uniform fogU $= Index1 (0.0::GLfloat)

-- cloudProgram :: IO (ResourcesClosure -> IO ())
-- cloudProgram = do
--     let builder =
--             forM_ simpleCube $ \(x,y,z) -> do
--                 bColor4 (x,y,z,0)
--                 bVertex3 (x,y+20,z)
--     program <- loadProgramSafe' "shaders/clouds.vert" "shaders/clouds.frag" noShader
-- 
--     stgen <- newStdGen
--     array3D <- SA.newListArray ((0,0,0,0),(3,64,64,64)) (map (fromIntegral . (`mod`256)) $ (randoms stgen::[Int]))
-- 
--     SA.withStorableArray array3D $ \ptr3D -> do
--         density <- makeTexture3D >>= textureFromPointer3D ptr3D (64,64,64)
-- 
--         obj' <- newDefaultGlyphObjectWithClosure builder () $ \_ -> do
--                     currentProgram $= Just program
--         [mvMatU, pMatU, densityU, globalAmbientU,lightposU] <- mapM (get . uniformLocation program)
--             ["mvMatrix","pMatrix","density","globalAmbient","lightpos"]
--         return $ \rc -> do
--             draw $ prepare obj' $ \_ -> do
--                 cullFace $= Nothing
--                 uniform mvMatU $= rcMVMatrix rc
--                 uniform pMatU $= rcPMatrix rc
--                 uniform globalAmbientU $= rcGlobalAmbient rc
--                 uniform lightposU $= rcLightPos rc
--                 setupTexturing3D density densityU 0
            

buildForestObject :: Seq.Seq GLfloat -> String -> String -> IO (ResourcesClosure -> IO ())
buildForestObject a_seq obj tex =
    if Seq.null a_seq then return ((const.return) ()) else do
    let bufferIO :: IO BufferObject
        bufferIO = (newArray . Fold.toList) a_seq >>= ptrToBuffer ArrayBuffer (Seq.length a_seq * 4)
    
    !buffer <- bufferIO
    (log',file) <- loadObjFile obj :: IO ([String],ObjectFile GLfloat)
    mapM_ putStrLn log'
    let !treeF = trace "build tree" $ (basicBuildObject file :: BuilderM GLfloat ())

    forestProg <- loadProgramSafe' 
        "shaders/forest.vert" "shaders/forest.frag" noShader

    woodTexture <- load tex >>= textureFromSurface
    let (dx,dy) = (mapT2 $ (1/).fromIntegral) (textureSize woodTexture)
    dXlocation <- get $ uniformLocation forestProg "dX"
    dYlocation <- get $ uniformLocation forestProg "dY"

    [textureU,lightU,globalAmbientU,pjMatrixU,mvMatrixU,timeU,normalMatrixU] <-
        getUniformsSafe forestProg ["texture","light","globalAmbient","pjMatrix","mvMatrix","time","normalMatrix"]

    obj' <- newDefaultGlyphObjectWithClosure treeF () $ \_ -> do
                currentProgram $= Just forestProg
                setupTexturing woodTexture textureU 0
                uniform dXlocation $= (Index1 $ (dx::GLfloat))
                uniform dYlocation $= (Index1 $ (dy::GLfloat))

                bindBuffer ArrayBuffer $= Just buffer

                let declareAttr location nelem' offset = do
                            vertexAttribPointer location $=
                                (ToFloat, VertexArrayDescriptor
                                    nelem' Float (fromIntegral $ (3+3+2+1+1)*sizeOf (0::GLfloat))
                                    (wordPtrToPtr offset))
                            vertexAttribArray location $= Enabled
                            vertexAttributeDivisor location SV.$= 1

                declareAttr (AttribLocation 10) 3 0
                declareAttr (AttribLocation 11) 3 (3*4)
                declareAttr (AttribLocation 12) 2 (6*4)
                declareAttr (AttribLocation 13) 1 (8*4)
                declareAttr (AttribLocation 14) 1 (9*4)

                printErrors "forestClosure"
    putStrLn $ "N trees = " ++! (Seq.length a_seq `div` 3)
    let obj'' = setNumInstances (Seq.length a_seq `div` 3) obj'

    return $ \rc -> do
        draw $ (prepare obj'') $ \_ -> do
            uniform mvMatrixU $= rcMVMatrix rc
            uniform pjMatrixU $= rcPMatrix rc
            uniform lightU $= rcLightPos rc
            uniform timeU $= (Index1 $ rcTime rc)
            uniform normalMatrixU $= rcNormalMatrix rc
            uniform globalAmbientU $= rcGlobalAmbient rc

buildWaterObject :: BuilderM GLfloat a -> IO (ResourcesClosure -> IO ())
buildWaterObject builder = do
    waterProg <- loadProgramFullSafe' 
         (Just ("shaders/water.tcs","shaders/water.tes"))
         noShader "shaders/water.vert" "shaders/water.frag"
    waterTexture <- load "textures/water.jpg" >>= textureFromSurface
    skyTexture <- load "textures/skybox_top.png" >>= textureFromSurface
    skyNightTexture <- load "textures/skybox_top_night.png" >>= textureFromSurface
    location <- get (uniformLocation waterProg "texture")
    skyLocation <- get (uniformLocation waterProg "skytex") 
    skyNightLocation <- get (uniformLocation waterProg "skynight")
    obj <- (liftM (setPrimitiveMode Ex.Patches) $ newDefaultGlyphObjectWithClosure builder () $ \_ -> do
                currentProgram $= Just waterProg
                setupTexturing waterTexture location 0
                setupTexturing skyTexture skyLocation 1
                setupTexturing skyNightTexture skyNightLocation 2
            )
    [fogU] <- getUniformsSafe waterProg ["fog"]
    return $ \rc -> do
        draw $ prepare obj $ \_ -> do
            cullFace $= Nothing
            patchVertices SV.$= (4::Int)
            uniform (UniformLocation 4) $= rcPMatrix rc 
            uniform (UniformLocation 5) $= rcMVMatrix rc
            uniform (UniformLocation 7) $= rcNormalMatrix rc
            uniform (UniformLocation 8) $= rcLightPos rc
            uniform (UniformLocation 9) $= Index1 (rcTime rc / 20.0)
            uniform (UniformLocation 10) $= rcGlobalAmbient rc
            bool <- (resourcesUnderWater $ rcResources rc)
            if bool then
                uniform fogU $= Index1 (0.9::GLfloat) else
                uniform fogU $= Index1 (0.0::GLfloat)
    

makeResources :: SDL.Surface -> BuilderM GLfloat b ->
    Seq.Seq GLfloat -> Seq.Seq GLfloat ->
    BuilderM GLfloat a -> Array (Int,Int) Tile ->
    ArrIO.IOArray (Int,Int) GLfloat -> IO Resources
makeResources surf builder forestB jungleB water arr waterarr = do
    let pMatrix' = perspectiveMatrix 50 1.8 0.1 100

    let l_routines = sequence [
            skyboxObject,
            (return $ \_ -> do
                vertexProgramPointSize $= Enabled
                depthFunc $= Just Less),
            buildTerrainObject builder,
            (return $ \_-> do
                blend $= Enabled
                cullFace $= Just Back
                blendFunc $= (GL.SrcAlpha,OneMinusSrcAlpha)),
            buildForestObject forestB "tree.obj" "textures/wood_low.png",
            buildForestObject jungleB "jungletree.obj" "textures/jungle_tree.png",
            buildWaterObject water
            -- cloudProgram
         ]
    Resources 
        <$> pure surf
        <*> do CameraPosition
                <$> pure (Vec3 (10,10,2))
                <*> pure 0
                <*> pure 0
        <*> do CameraPosition
                <$> pure (Vec3 (0,0,0))
                <*> pure 0
                <*> pure 0
        <*> pure pMatrix'
        <*> pure pMatrix'
        <*> l_routines
        <*> pure 0
        <*> pure 1
        <*> pure 0
        <*> pure arr
        <*> pure return
        <*> pure 1
        <*> pure 0
        <*> pure waterarr

printErrors :: String -> IO ()
printErrors ctx =
    get errors >>= mapM_ (putStrLn . (("GL["++ctx++"]: ")++) . show)

skyboxSides :: GLfloat -> BuilderM GLfloat ()
skyboxSides dist = do
    let q = trianglesFromQuads $
                -- back
                [(bTexture2(0,0),    bVertex3 (-dist,  dist, -dist)),
                 (bTexture2(0.25,0), bVertex3 ( dist,  dist, -dist)),
                 (bTexture2(0.25,1), bVertex3 ( dist, -dist, -dist)),
                 (bTexture2(0,1),    bVertex3 (-dist, -dist, -dist))] ++
                
                -- front
                [(bTexture2(0.75,0),    bVertex3 (-dist,  dist, dist)),
                 (bTexture2(0.5,0),   bVertex3 ( dist,  dist, dist)),
                 (bTexture2(0.5,1),   bVertex3 ( dist, -dist, dist)),
                 (bTexture2(0.75,1),    bVertex3 (-dist, -dist, dist))] ++

                -- right
                [(bTexture2(0.75,1),    bVertex3 (-dist, -dist,  dist)),
                 (bTexture2(0.75,0),   bVertex3 (-dist,  dist,  dist)),
                 (bTexture2(1.0,0),   bVertex3 (-dist,  dist, -dist)),
                 (bTexture2(1.0,1),    bVertex3 (-dist, -dist, -dist))] ++

                -- left
                [(bTexture2(0.5,1),    bVertex3 ( dist, -dist,  dist)),
                 (bTexture2(0.5,0),    bVertex3 ( dist,  dist,  dist)),
                 (bTexture2(0.25,0) ,    bVertex3 ( dist,  dist, -dist)),
                 (bTexture2(0.25,1) ,    bVertex3 ( dist, -dist, -dist))]

            in
        mapM_ (uncurry (>>)) q
skyboxTop :: GLfloat -> BuilderM GLfloat ()
skyboxTop dist = do
        mapM_ (uncurry (>>)) $
            trianglesFromQuads
                [(bTexture2(1,0),   bVertex3 ( -dist, dist,  dist)),
                 (bTexture2(1,1),   bVertex3 (  dist, dist,  dist)),
                 (bTexture2(0,1),   bVertex3 (  dist, dist, -dist)),
                 (bTexture2(0,0),   bVertex3 ( -dist, dist, -dist))]

skyboxObject :: IO (ResourcesClosure -> IO ())
skyboxObject = do
    prog <- loadProgramSafe' "shaders/sky.vert" "shaders/sky.frag" (Nothing::Maybe String)
    texLoc <- get $ uniformLocation prog "texture"
    texLocNight <- get $ uniformLocation prog "night_tex"
    matLoc <- get $ uniformLocation prog "mvMatrix"
    pmatLoc <- get $ uniformLocation prog "pjMatrix"

    glTexParameteri gl_TEXTURE_2D gl_TEXTURE_WRAP_S $ fromIntegral gl_CLAMP_TO_EDGE
    glTexParameteri gl_TEXTURE_2D gl_TEXTURE_WRAP_T $ fromIntegral gl_CLAMP_TO_EDGE
    l_texture <- load "textures/skybox_sides.png" >>= textureFromSurface
    glTexParameteri gl_TEXTURE_2D gl_TEXTURE_WRAP_S $ fromIntegral gl_CLAMP_TO_EDGE
    glTexParameteri gl_TEXTURE_2D gl_TEXTURE_WRAP_T $ fromIntegral gl_CLAMP_TO_EDGE
    l_texture2 <- load "textures/skybox_sides_night.png" >>= textureFromSurface
    glTexParameteri gl_TEXTURE_2D gl_TEXTURE_WRAP_S $ fromIntegral gl_CLAMP_TO_EDGE
    glTexParameteri gl_TEXTURE_2D gl_TEXTURE_WRAP_T $ fromIntegral gl_CLAMP_TO_EDGE
    l_textureTop <- load "textures/skybox_top.png" >>= textureFromSurface
    glTexParameteri gl_TEXTURE_2D gl_TEXTURE_WRAP_S $ fromIntegral gl_CLAMP_TO_EDGE
    glTexParameteri gl_TEXTURE_2D gl_TEXTURE_WRAP_T $ fromIntegral gl_CLAMP_TO_EDGE
    l_textureTopNight <- load "textures/skybox_top_night.png" >>= textureFromSurface

    [lightposU,multU] <- mapM (get . uniformLocation prog) 
                    ["lightpos","mult"]
    topObj <- newDefaultGlyphObjectWithClosure (skyboxTop 1) () $ \_ -> do
        setupTexturing l_textureTop texLoc 2
        setupTexturing l_textureTopNight texLocNight 3

    obj <- newDefaultGlyphObjectWithClosure (skyboxSides 1) (matLoc,pmatLoc) $ \_ -> do
        currentProgram $= Just prog
        setupTexturing l_texture texLoc 0
        setupTexturing l_texture2 texLocNight 1
        printErrors "Skybox"

    let obj' = teardown obj $ \_ -> do
        draw topObj
    return $ \rc -> do
        depthFunc $= Nothing
        cullFace $= Nothing
        draw $ prepare obj' $ \this -> do
            let (l_matLoc,l_pmatLoc) = getResources this
            let (CameraPosition _ th ph) = rcCameraPos rc
            uniform lightposU $= rcLightPos rc
            uniform l_pmatLoc $= rcPMatrix rc
            uniform l_matLoc $= buildMVMatrix (CameraPosition (Vec3 (0,0,0)) th ph)
            uniform (UniformLocation 1) $= rcGlobalAmbient rc
            bool <- (resourcesUnderWater $ rcResources rc)
            if bool then
                uniform multU $= Index1 (0.0::GLfloat) else
                uniform multU $= Index1 (1.0::GLfloat)
                
    

prepareSkybox :: Mat4 GLfloat -> Mat4 GLfloat -> GlyphObject (Mat4 GLfloat -> Mat4 GLfloat -> IO ()) -> IO ()
prepareSkybox proj lookat obj = do
    (getResources obj) proj lookat

