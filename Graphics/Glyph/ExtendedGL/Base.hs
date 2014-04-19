{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Graphics.Glyph.ExtendedGL.Base where

import qualified Graphics.Rendering.OpenGL as GL
import Graphics.Rendering.OpenGL.Raw.Core31
import Graphics.Rendering.OpenGL.Raw.ARB

import Foreign.Marshal.Alloc
import Foreign.Ptr
import Foreign.Storable
import Foreign.C.Types

import System.IO.Unsafe
import Control.Monad

import Data.StateVar
import Unsafe.Coerce

data ExPrimitiveMode = Points | Triangles | Lines | Patches deriving (Show,Enum)

class IsBindable a where
    bind :: a -> IO ()
class IsGLEnumMarshallable a where
    toGLEnum :: a -> GLenum
class IsGenerable a where
    generate :: IO a
class IsWrappedPrimitive t a where
    unwrap :: a -> t
    wrap :: t -> a
class HasIntegerParam t a where
    parami :: t -> a -> SettableStateVar GLuint
class HasFloatParam t a where
    paramf :: t -> a -> SettableStateVar GLfloat
class HasParamOfType b t a where
    param :: t -> a -> SettableStateVar b

class IsPrimitiveModeMarshallable a where
    marshalPrimitiveMode :: a -> GLuint

castPrimitive :: forall a b t. (IsWrappedPrimitive t a, IsWrappedPrimitive t b) => a -> b
castPrimitive x = wrap unw
    where
        unw :: t
        unw = unwrap x

instance (IsWrappedPrimitive a a) where
    unwrap = id
    wrap = id
instance (IsWrappedPrimitive GLenum a) => IsGLEnumMarshallable a where
    toGLEnum = unwrap

instance IsPrimitiveModeMarshallable ExPrimitiveMode where
    marshalPrimitiveMode x = case x of
        Points    -> gl_POINTS
        Triangles -> gl_TRIANGLES
        Lines     -> gl_LINES
        Patches   -> gl_PATCHES

instance IsPrimitiveModeMarshallable GL.PrimitiveMode where
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

instance IsPrimitiveModeMarshallable GLuint where
    marshalPrimitiveMode = id

drawArraysInstanced ::
 (IsPrimitiveModeMarshallable a) => 
    a -> GL.ArrayIndex ->
    GL.NumArrayIndices ->
    GLsizei -> IO ()
drawArraysInstanced = glDrawArraysInstanced . marshalPrimitiveMode

vertexAttributeDivisor :: GL.AttribLocation -> SettableStateVar GLuint
vertexAttributeDivisor (GL.AttribLocation loc) =
    makeSettableStateVar $ \val ->
        glVertexAttribDivisor loc val

{- Sets the number of vertices per patch
 - for OpenGL -}
patchVertices :: (Integral a) => SettableStateVar a
patchVertices = 
    makeSettableStateVar $ \val ->
        glPatchParameteri gl_PATCH_VERTICES $ fromIntegral val

{- Returns the maximum number of patches
 - for a tessilation shader -}
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

coerced :: a
coerced = unsafeCoerce (0::Int)
