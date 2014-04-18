module Graphics.Glyph.ExtendedGL where

import Graphics.Rendering.OpenGL hiding (Points,Lines,Triangles)
import qualified Graphics.Rendering.OpenGL as GL
import Graphics.Rendering.OpenGL.Raw.Core31
import Graphics.Rendering.OpenGL.Raw.ARB

import Foreign.Marshal.Alloc
import Foreign.Ptr
import Foreign.Storable
import Foreign.C.Types

import System.IO.Unsafe
import Control.Monad

data ExPrimitiveMode = Points | Triangles | Lines | Patches deriving (Show,Enum)

class IsPrimitiveModeMarshallable a where
    marshalPrimitiveMode :: a -> GLuint

instance IsPrimitiveModeMarshallable ExPrimitiveMode where
    marshalPrimitiveMode x = case x of
        Points    -> gl_POINTS
        Triangles -> gl_TRIANGLES
        Lines     -> gl_LINES
        Patches   -> gl_PATCHES

instance IsPrimitiveModeMarshallable PrimitiveMode where
    marshalPrimitiveMode x = case x of
        GL.Points -> 0x0
        GL.Lines -> 0x1
        GL.LineLoop -> 0x2
        GL.LineStrip -> 0x3
        GL.Triangles -> 0x4
        GL.TriangleStrip -> 0x5
        GL.TriangleFan -> 0x6
        GL.Quads -> 0x7
        GL.QuadStrip -> 0x8
        GL.Polygon -> 0x9

drawArraysInstanced :: 
    (IsPrimitiveModeMarshallable a) => a -> ArrayIndex -> NumArrayIndices -> GLsizei -> IO ()
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
