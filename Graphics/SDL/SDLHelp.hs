module Graphics.SDL.SDLHelp where

import Graphics.UI.SDL.Image as SDLImg
import Graphics.UI.SDL as SDL 
import Data.Word
import Control.Monad
import Graphics.Glyph.Util

import Graphics.Rendering.OpenGL as GL
import Graphics.Rendering.OpenGL.Raw.Core31

import Foreign.Storable
import Foreign.Ptr
import Data.Bits

import System.IO.Unsafe
import System.Endian
import System.Exit

data TextureData = TextureData {
    textureSize :: (Int,Int),
    textureObject :: TextureObject } deriving Show

data TextureData3D = TextureData3D {
    textureSize3D :: (Int,Int,Int),
    textureObject3D :: TextureObject } deriving Show


bindSurfaceToTexture :: SDL.Surface -> TextureObject -> IO TextureData
bindSurfaceToTexture surf to = do
    textureBinding Texture2D $= Just to
    bbp <- liftM fromIntegral (pixelFormatGetBytesPerPixel $ surfaceGetPixelFormat surf)
    putStrLn $ "bpp: " ++! bbp
    ptr <- surfaceGetPixels surf
    glTexImage2D gl_TEXTURE_2D 0 bbp (w surf) (h surf) 0 (if bbp == 3 then gl_RGB else gl_RGBA) gl_UNSIGNED_BYTE ptr
    return $ TextureData (w surf, h surf) to
    where 
          w :: (Integral a) => SDL.Surface -> a
          w = fromIntegral . surfaceGetWidth
          h :: (Integral a) => SDL.Surface -> a
          h = fromIntegral . surfaceGetHeight

textureFromPointer3D :: Ptr Word8 -> (Int,Int,Int) -> TextureObject -> IO TextureData3D
textureFromPointer3D ptr (w,h,d) to = do
        textureBinding Texture3D $= Just to
        glTexImage3D gl_TEXTURE_3D 0 3 (f w) (f h) (f d) 0 gl_RGB gl_UNSIGNED_BYTE ptr
        return $ TextureData3D (w,h,d) to
    where f = fromIntegral

textureFromSurface :: SDL.Surface -> IO TextureData
textureFromSurface surf = makeTexture >>= (bindSurfaceToTexture surf >=> return)

makeTexture3D :: IO TextureObject
makeTexture3D = do
    texobj <- liftM head $ genObjectNames 1
    textureBinding Texture3D $= Just texobj
    textureFilter Texture3D $= ((Linear', Nothing), Linear')
    return texobj

makeTexture :: IO TextureObject
makeTexture = do
    texobj <- liftM head $ genObjectNames 1
    textureBinding Texture2D $= Just texobj
    textureFilter Texture2D $= ((Linear', Nothing), Linear')
    return texobj

getPixel :: Int -> Int -> SDL.Surface -> IO Word32
getPixel x y surf = do
    bpp <- liftM fromIntegral (pixelFormatGetBytesPerPixel $ surfaceGetPixelFormat surf)
    ptr <- (surfaceGetPixels surf >>= return.castPtr) :: IO (Ptr Word8)
    let newPtr = ptr `plusPtr` (y * (fromIntegral $ surfaceGetPitch surf)) `plusPtr` (x * bpp)
    
    ret <- case bpp of
        -- bytes = R G B A
        1 -> liftM fromIntegral $ peek (castPtr newPtr :: Ptr Word8)
        2 -> liftM fromIntegral $ peek (castPtr newPtr :: Ptr Word16)
        3 -> do
            ord1 <- liftM fromIntegral $ peek (castPtr newPtr :: Ptr Word16)
            ord2 <- liftM fromIntegral $ peek (castPtr (newPtr `plusPtr` 2) :: Ptr Word8)
            return $ ((ord1 `shiftL` 16) + (ord2 `shiftL` 8)) + 0xFF
        4 -> do
            liftM fromIntegral $ peek (castPtr newPtr :: Ptr Word32)
        _ -> error "Unrecognized format"

    return $ toBE32 ret

getPixelUnsafe :: Int -> Int -> SDL.Surface -> Word32
getPixelUnsafe x y surf = unsafePerformIO $ getPixel x y surf

rgbToWord :: Word8 -> Word8 -> Word8 -> Word32
rgbToWord r g b =
    let tW32 x = (fromIntegral x) :: Word32 in
    ( (tW32 r) `shiftL` 24) +
    ( (tW32 g) `shiftL` 16) +
    ( (tW32 b) `shiftL`  8) +
    0xFF 

wordToPixel :: Word32 -> Color4 Word8
wordToPixel word =
        Color4 (fromIntegral $ word .&. 0xFF)
               (fromIntegral $ (word `shiftR` 8) .&. 0xFF)
               (fromIntegral $ (word `shiftR` 16) .&. 0xFF)
               (fromIntegral $ (word `shiftR` 24) .&. 0xFF)

getRGBA :: SDL.Surface -> Int -> Int -> IO (Color4 Word8)
getRGBA surf x y = liftM wordToPixel $ getPixel x y surf

simpleStartup :: String -> (Int,Int) -> IO Surface
simpleStartup name' (w,h) = do
        SDL.init [SDL.InitEverything]
        SDL.setVideoMode w h 32 [SDL.OpenGL, SDL.Resizable, SDL.DoubleBuf]
        SDL.setCaption name' name'
        SDL.getVideoSurface

defaultReshape :: Int -> Int -> a -> IO a
defaultReshape w h ret = do
    let size = Size (fromIntegral w) (fromIntegral h)
    viewport $=(Position 0 0, size)
    _ <- SDL.setVideoMode w h 32 [SDL.OpenGL, SDL.Resizable, SDL.DoubleBuf]
    return ret

startPipeline :: (Int -> Int -> a -> IO a) -> (Event -> a -> IO a) -> (a -> IO a) -> (a -> IO a) -> a -> IO ()
startPipeline reshapeH eventH displayH updateH ini = do
    let pumpEvents' res = do
        ev <- SDL.pollEvent
        case ev of
            Quit -> do
                putStrLn "Exit event."
                exitSuccess
            SDL.NoEvent -> return res
            VideoResize w h -> reshapeH w h res >>= pumpEvents'
            _ -> eventH ev res >>= pumpEvents'
    let runPipeline val = do
            res <- pumpEvents' val >>= displayH
            updateH res >>= runPipeline

    -- TODO unhardcode this
    reshapeH 640 480 ini >>= runPipeline

setupTexturing :: TextureData -> UniformLocation -> Int -> IO ()
setupTexturing (TextureData _ to) tu unit = do
    texture Texture2D $= Enabled
    activeTexture $= TextureUnit (fromIntegral unit)
    textureBinding Texture2D $= Just to
    uniform tu $= Index1 (fromIntegral unit::GLint)

setupTexturing3D :: TextureData3D -> UniformLocation -> Int -> IO ()
setupTexturing3D (TextureData3D _ to) tu unit = do
    texture Texture3D $= Enabled
    activeTexture $= TextureUnit (fromIntegral unit)
    textureBinding Texture3D $= Just to
    uniform tu $= Index1 (fromIntegral unit::GLint)
