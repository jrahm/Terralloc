module Graphics.Rendering.HelpGL 
( emptyRGBATexture )
where

import Graphics.Rendering.OpenGL as GL
import Graphics.Rendering.OpenGL.Raw.Core31
import Foreign.Ptr

import Foreign.Marshal.Array

(?) :: (Integral a, Num b) => () -> a -> b
(?) _ = fromIntegral


emptyRGBATexture :: Int -> Int -> IO ()
emptyRGBATexture w h =
    texImage2D Texture2D NoProxy 0 RGBA' (TextureSize2D (()?w) (()?h)) 0 (PixelData RGBA UnsignedByte nullPtr)
