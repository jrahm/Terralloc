module Graphics.Glyph.ExtendedGL where

import Graphics.Rendering.OpenGL
import Graphics.Rendering.OpenGL.Raw.Core31
import Graphics.Rendering.OpenGL.Raw.ARB

import Foreign.Marshal.Alloc
import Foreign.Ptr
import Foreign.Storable
import Foreign.C.Types

import System.IO.Unsafe
import Control.Monad

marshalPrimitiveMode :: PrimitiveMode -> GLenum
marshalPrimitiveMode x = case x of
   Points -> 0x0
   Lines -> 0x1
   LineLoop -> 0x2
   LineStrip -> 0x3
   Triangles -> 0x4
   TriangleStrip -> 0x5
   TriangleFan -> 0x6
   Quads -> 0x7
   QuadStrip -> 0x8
   Polygon -> 0x9

drawArraysInstanced :: PrimitiveMode -> ArrayIndex -> NumArrayIndices -> GLsizei -> IO ()
drawArraysInstanced = glDrawArraysInstanced . marshalPrimitiveMode

vertexAttributeDivisor :: AttribLocation -> SettableStateVar GLuint
vertexAttributeDivisor (AttribLocation loc) =
    makeSettableStateVar $ \val ->
        glVertexAttribDivisor loc val

patchVertices :: (Integral a) => SettableStateVar a
patchVertices = 
    makeSettableStateVar $ \val ->
        glPatchParameteri gl_PATCH_VERTICES $ fromIntegral val

maxPatchVertices :: IO CInt
maxPatchVertices =
        alloca $ \ptr -> do
            glGetIntegerv gl_MAX_PATCH_VERTICES ptr
            peek ptr

getGLVersion :: IO String
getGLVersion =
    let lift2 (a,b) = do
            x <- a ; y <- b ; return (x,y)
             in
    alloca $ \ptr1 -> alloca $ \ptr2 -> do
        glGetIntegerv gl_MAJOR_VERSION ptr1
        glGetIntegerv gl_MINOR_VERSION ptr2
        (v1,v2) <- lift2 (peek ptr1, peek ptr2)
        return ("OpenGL " ++ show v1 ++ "." ++ show v2)
