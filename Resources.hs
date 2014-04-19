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
import Graphics.Glyph.GeometryBuilder as GB
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
import Data.Sequence ((><),(|>),(<|))
import qualified Data.Foldable as Fold
import Data.Maybe
import Debug.Trace

import Foreign.Marshal.Array

import System.Exit
import System.FilePath

import Models
import Debug.Trace

import qualified Data.StateVar as SV

data CameraPosition = CameraPosition {
    pEye :: Vec3 GLfloat,
    pTh :: GLfloat,
    pPh :: GLfloat
} deriving Show

data ObjectData = ObjectData Program

data Resources = Resources {
    rSurface :: SDL.Surface,

    rPosition :: CameraPosition,
    rDPosition :: CameraPosition,

    pMatrix :: Mat4 GLfloat,
    mvMatrix :: Mat4 GLfloat,

<<<<<<< HEAD
    routines :: [ResourcesClosure -> IO ()],
    -- object :: GlyphObject (),
    -- forest :: GlyphObject (),
    -- jungle :: GlyphObject (),
    -- waterObj  :: GlyphObject (),
=======
    object :: GlyphObject (),
    forest :: Maybe (GlyphObject ()),
    jungle :: Maybe (GlyphObject ()),
    waterObj  :: GlyphObject (),
>>>>>>> a2224be33baae7ae07473e74fe94414cdb8f41d2

    speed :: Int,
    timeSpeed :: Int,
    time :: Int
}

data ResourcesClosure = ResourcesClosure {
      rcMVMatrix :: Mat4 GLfloat
    , rcPMatrix  :: Mat4 GLfloat
    , rcLightPos :: Vec4 GLfloat
    , rcTime     :: GLfloat
    , rcNormalMatrix  :: Mat3 GLfloat
    , rcGlobalAmbient :: Vec4 GLfloat
    , rcCameraPos :: CameraPosition
}

$(declareSetters ''Resources)

buildMVMatrix :: CameraPosition -> Mat4 GLfloat
buildMVMatrix (CameraPosition eye th ph) =
    let up = if ph' >= 90 && ph' < 270 then Vec3 (0,-1,0) else Vec3 (0,1,0)
            where ph' = (floor ph::Int) `mod` 360 in
    let lookat = eye <+> (Vec3 $ toEuclidian (1,th,ph)) in
        lookAtMatrix eye lookat up

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
            return $ setSpeed (speed res + 1) res
        KeyDown (Keysym SDLK_s _ _) ->
            return $ setSpeed (speed res - 1) res
        KeyUp (Keysym SDLK_w _ _) ->
            return $ setSpeed (speed res - 1) res
        KeyUp (Keysym SDLK_s _ _) ->
            return $ setSpeed (speed res + 1) res

        KeyUp (Keysym SDLK_g _ _) -> do
            SDL.showCursor False
            SDL.grabInput True
            return res
        KeyUp (Keysym SDLK_f _ _) -> do
            ret <- reshape 1920 1080 res
            SDL.toggleFullscreen $ rSurface ret
            SDL.showCursor False
            SDL.grabInput True
            return ret

        _ -> return res

displayHandle :: Resources -> IO Resources
displayHandle resources = do
    let cameraPos@(CameraPosition _ th ph) = rPosition resources
    let lighty = ((/10) . fromIntegral . time) resources
    let logist c =  (1 / (1 + 2.71828**(-c*x))) * 0.9 + 0.1
            where x = sine $ Degrees (lighty)
    let globalAmbient::(GLfloat,GLfloat,GLfloat,GLfloat)
        globalAmbient@(r,g,b,a)= ( logist 2+0.1, logist 10, (logist 15) + 0.1,(sine.Degrees) lighty)
    let lightPos = Vec4( 50,
                         1000000 * (sine.Degrees $ lighty),
                         -1000000 * (cosine.Degrees . (/10) . fromIntegral . time) resources,
                         1 )
    let l_mvMatrix = buildMVMatrix $ cameraPos
    let normalMatrix = glslModelViewToNormalMatrix l_mvMatrix

<<<<<<< HEAD
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
                in mapM_ (Prelude.$rc) $ routines resources
=======
    cullFace $= Just Front
    draw $ prepare (object resources) $ \_ -> do
        uniform (UniformLocation 5) $= l_mvMatrix
        uniform (UniformLocation 4) $= pMatrix resources
        uniform (UniformLocation 6) $= l_mvMatrix `glslMatMul` lightPos
        uniform (UniformLocation 7) $= normalMatrix
        uniform (UniformLocation 8) $= Vec4 (r,g,b,a::GLfloat)
        return ()

    blend $= Enabled
    cullFace $= Just Back
    blendFunc $= (GL.SrcAlpha,OneMinusSrcAlpha)

    when (isJust $ forest resources) $
        draw $ prepare (fromJust $ forest resources) $ \_ -> do
            uniform (UniformLocation 5) $= l_mvMatrix
            uniform (UniformLocation 4) $= pMatrix resources
            uniform (UniformLocation 7) $= l_mvMatrix `glslMatMul` lightPos
            uniform (UniformLocation 8) $= Index1 (fromIntegral $ time resources::GLfloat)
            uniform (UniformLocation 9) $= normalMatrix
    
            uniform (UniformLocation 10) $= Vec4 (r,g,b,a::GLfloat)
            return ()

    when (isJust $ jungle resources) $ do
        draw $ prepare (fromJust $ jungle resources) $ \_ -> do
            uniform (UniformLocation 5) $= l_mvMatrix
            uniform (UniformLocation 4) $= pMatrix resources
            uniform (UniformLocation 7) $= l_mvMatrix `glslMatMul` lightPos
            uniform (UniformLocation 8) $= Index1 (fromIntegral $ time resources::GLfloat)
            uniform (UniformLocation 9) $= normalMatrix
    
            uniform (UniformLocation 10) $= Vec4 (r,g,b,a::GLfloat)
            return ()

    cullFace $= Nothing
    draw $ prepare (waterObj resources) $ \_ -> do
        patchVertices SV.$= 4
        uniform (UniformLocation 4) $= pMatrix resources
        uniform (UniformLocation 5) $= l_mvMatrix
        uniform (UniformLocation 7) $= normalMatrix
        uniform (UniformLocation 8) $= l_mvMatrix `glslMatMul` lightPos
        uniform (UniformLocation 9) $= Index1 ((fromIntegral $ time resources) / 20::GLfloat)
        uniform (UniformLocation 10) $= Vec4 (r,g,b,a::GLfloat)
        return ()
>>>>>>> a2224be33baae7ae07473e74fe94414cdb8f41d2

    SDL.glSwapBuffers
    return resources

updateHandle :: Resources -> IO Resources
updateHandle res = do
   return $ setRPosition (rPosition res `cAdd` rDPosition res) $
            let new = ((+) `on` (Prelude.$ res)) timeSpeed time  in
                setTime new res
   where (CameraPosition x y z) `cAdd` (CameraPosition _ y' z') =
           let fri = fromIntegral
               x' = (fri $ speed res) `vScale` (V.normalize $ Vec3 $ toEuclidian (1,y, z)) in
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
<<<<<<< HEAD
    return $ \rc -> do
        draw $ prepare obj $ \_ -> do
            cullFace $= Just Front
            uniform (UniformLocation 5) $= rcMVMatrix rc
            uniform (UniformLocation 4) $= rcPMatrix rc
            uniform (UniformLocation 6) $= rcLightPos rc
            uniform (UniformLocation 7) $= rcNormalMatrix rc
            uniform (UniformLocation 8) $= rcGlobalAmbient rc

buildForestObject :: Seq.Seq GLfloat -> String -> String -> IO (ResourcesClosure -> IO ())
buildForestObject seq obj tex =
    if Seq.null seq then return ((const.return) ()) else do
=======

buildForestObject :: Seq.Seq GLfloat -> String -> String -> IO (Maybe (GlyphObject ()))
buildForestObject seq obj tex =
    if Seq.null seq then return Nothing else liftM Just $
    do
>>>>>>> a2224be33baae7ae07473e74fe94414cdb8f41d2
    let bufferIO :: IO BufferObject
        bufferIO = (newArray . Fold.toList) seq >>= ptrToBuffer ArrayBuffer (Seq.length seq * 4)
    
    !buffer <- bufferIO
    (log',file) <- loadObjFile obj :: IO ([String],ObjectFile GLfloat)
    mapM_ putStrLn log'
    let !treeF = trace "build tree" $ (basicBuildObject file :: BuilderM GLfloat ())

    forestProg <- loadProgramSafe' 
        "shaders/forest.vert" "shaders/forest.frag" (Nothing::Maybe String)

    woodTexture <- load tex >>= textureFromSurface
    let (dx,dy) = (mapT2 $ (1/).fromIntegral) (textureSize woodTexture)
    dXlocation <- get $ uniformLocation forestProg "dX"
    dYlocation <- get $ uniformLocation forestProg "dY"

    obj' <- newDefaultGlyphObjectWithClosure treeF () $ \_ -> do
                currentProgram $= Just forestProg
                setupTexturing woodTexture (UniformLocation 6) 0
                uniform dXlocation $= (Index1 $ (dx::GLfloat))
                uniform dYlocation $= (Index1 $ (dy::GLfloat))

                bindBuffer ArrayBuffer $= Just buffer

                let declareAttr location nelem offset = do
                            vertexAttribPointer location $=
                                (ToFloat, VertexArrayDescriptor
                                    nelem Float (fromIntegral $ (3+3+2+1)*sizeOf (0::GLfloat))
                                    (wordPtrToPtr offset))
                            vertexAttribArray location $= Enabled
                            vertexAttributeDivisor location SV.$= 1

                declareAttr (AttribLocation 10) 3 0
                declareAttr (AttribLocation 11) 3 (3*4)
                declareAttr (AttribLocation 12) 2 (6*4)
                declareAttr (AttribLocation 13) 1 (8*4)

                printErrors "forestClosure"
    putStrLn $ "N trees = " ++! (Seq.length seq `div` 3)
    let obj'' = setNumInstances (Seq.length seq `div` 3) obj'

    return $ \rc -> do
        draw $ (prepare obj'') $ \_ -> do
            uniform (UniformLocation 5) $= rcMVMatrix rc
            uniform (UniformLocation 4) $= rcPMatrix rc
            uniform (UniformLocation 7) $= rcLightPos rc
            uniform (UniformLocation 8) $= (Index1 $ rcTime rc)
            uniform (UniformLocation 9) $= rcNormalMatrix rc
            uniform (UniformLocation 10) $= rcGlobalAmbient rc

buildWaterObject :: BuilderM GLfloat a -> IO (ResourcesClosure -> IO ())
buildWaterObject builder = do
    waterProg <- loadProgramFullSafe' 
         (Just ("shaders/water.tcs","shaders/water.tes"))
         (Nothing::Maybe String) "shaders/water.vert" "shaders/water.frag"
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
    return $ \rc -> do
        draw $ prepare obj $ \_ -> do
            cullFace $= Nothing
            patchVertices SV.$= 4
            uniform (UniformLocation 4) $= rcPMatrix rc 
            uniform (UniformLocation 5) $= rcMVMatrix rc
            uniform (UniformLocation 7) $= rcNormalMatrix rc
            uniform (UniformLocation 8) $= rcLightPos rc
            uniform (UniformLocation 9) $= Index1 (rcTime rc / 20.0)
            uniform (UniformLocation 10) $= rcGlobalAmbient rc
    

makeResources :: SDL.Surface -> BuilderM GLfloat b ->
    Seq.Seq GLfloat -> Seq.Seq GLfloat ->
    BuilderM GLfloat a -> IO Resources
makeResources surf builder forestB jungleB water = do
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
    texture <- load "textures/skybox_sides.png" >>= textureFromSurface
    glTexParameteri gl_TEXTURE_2D gl_TEXTURE_WRAP_S $ fromIntegral gl_CLAMP_TO_EDGE
    glTexParameteri gl_TEXTURE_2D gl_TEXTURE_WRAP_T $ fromIntegral gl_CLAMP_TO_EDGE
    texture2 <- load "textures/skybox_sides_night.png" >>= textureFromSurface
    glTexParameteri gl_TEXTURE_2D gl_TEXTURE_WRAP_S $ fromIntegral gl_CLAMP_TO_EDGE
    glTexParameteri gl_TEXTURE_2D gl_TEXTURE_WRAP_T $ fromIntegral gl_CLAMP_TO_EDGE
    textureTop <- load "textures/skybox_top.png" >>= textureFromSurface
    glTexParameteri gl_TEXTURE_2D gl_TEXTURE_WRAP_S $ fromIntegral gl_CLAMP_TO_EDGE
    glTexParameteri gl_TEXTURE_2D gl_TEXTURE_WRAP_T $ fromIntegral gl_CLAMP_TO_EDGE
    textureTopNight <- load "textures/skybox_top_night.png" >>= textureFromSurface

    topObj <- newDefaultGlyphObjectWithClosure (skyboxTop 1) () $ \_ -> do
        setupTexturing textureTop texLoc 2
        setupTexturing textureTopNight texLocNight 3

    obj <- newDefaultGlyphObjectWithClosure (skyboxSides 1) (matLoc,pmatLoc) $ \_ -> do
        currentProgram $= Just prog
        setupTexturing texture texLoc 0
        setupTexturing texture2 texLocNight 1
        printErrors "Skybox"

    let obj' = teardown obj $ \_ -> do
        draw topObj
    return $ \rc -> do
        depthFunc $= Nothing
        cullFace $= Nothing
        draw $ prepare obj' $ \this -> do
            let (matLoc,pmatLoc) = getResources this
            let (CameraPosition _ th ph) = rcCameraPos rc
            uniform pmatLoc $= rcPMatrix rc
            uniform matLoc $= buildMVMatrix (CameraPosition (Vec3 (0,0,0)) th ph)
            uniform (UniformLocation 1) $= rcGlobalAmbient rc
                
    

prepareSkybox :: Mat4 GLfloat -> Mat4 GLfloat -> GlyphObject (Mat4 GLfloat -> Mat4 GLfloat -> IO ()) -> IO ()
prepareSkybox proj lookat obj = do
    (getResources obj) proj lookat

