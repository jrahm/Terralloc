{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Graphics.Glyph.ExtendedGL.Framebuffers where

import Graphics.Rendering.OpenGL.Raw.ARB
import Graphics.Rendering.OpenGL.Raw.Core31
import qualified Graphics.Rendering.OpenGL as GL

import Graphics.Glyph.ExtendedGL.Base

import Foreign.Marshal.Alloc
import Foreign.Ptr
import Foreign.Storable
import Foreign.C.Types

import Data.StateVar
import Control.Monad

import Unsafe.Coerce


class (
    HasParamOfType GLuint FramebufferParameter a,
    HasIntegerParam GLenum a,
    IsGenerable a,
    IsBindable a, IsWrappedPrimitive GLuint a) => IsFramebuffer a where
    
    -- this function MUST discard the argument
    getType :: a -> GLenum

framebufferBasicParameteri :: (IsFramebuffer a) => GLenum -> a -> GLenum -> SettableStateVar GLuint
framebufferBasicParameteri typ fb enum =
    makeSettableStateVar (\value -> do
        bind fb
        glFramebufferParameteri typ enum $ fromIntegral value)

data Renderbuffer = Renderbuffer GLuint
instance IsWrappedPrimitive GLuint Renderbuffer where
    unwrap (Renderbuffer x) = x
instance IsGenerable Renderbuffer where
    generate = alloca $ \ptr -> do
        glGenRenderbuffers 1 ptr
        liftM Renderbuffer $ peek ptr
instance IsBindable Renderbuffer where
    bind = glBindRenderbuffer gl_RENDERBUFFER . unwrap

data RenderbufferArgument =
        DepthAttachment
instance IsWrappedPrimitive GLenum RenderbufferArgument where
    unwrap DepthAttachment = gl_DEPTH_ATTACHMENT

renderBufferStorageRaw :: (IsGLEnumMarshallable a, IsGLEnumMarshallable b) => a -> b -> Int -> Int -> IO ()
renderBufferStorageRaw typ enum w h = glRenderbufferStorage (toGLEnum typ) 
                                    (toGLEnum enum) (fromIntegral w) (fromIntegral h)
renderBufferStorage :: (IsGLEnumMarshallable a) => Renderbuffer -> SettableStateVar (a,Int,Int)
renderBufferStorage buffer = makeSettableStateVar $ \(en,w,h) -> do
                                bind buffer
                                renderBufferStorageRaw gl_RENDERBUFFER en w h

frameBufferRenderBuffer :: forall a b. (IsFramebuffer a,IsGLEnumMarshallable b) => Renderbuffer -> b -> IO a
frameBufferRenderBuffer rb e = do
    let enum :: GLenum
        enum = getType test
        unw :: GLuint
        unw = unwrap rb
    bind rb
    glFramebufferRenderbuffer enum (toGLEnum e) gl_RENDERBUFFER (unwrap rb)
    return $ wrap unw
    where
        test :: a
        test = coerced

data DrawFramebuffer = DrawFramebuffer GLuint
data FramebufferParameter = DefaultWidth | DefaultHeight

instance IsWrappedPrimitive GLenum FramebufferParameter where
    unwrap p = case p of
        DefaultWidth -> gl_FRAMEBUFFER_DEFAULT_WIDTH
        DefaultHeight -> gl_FRAMEBUFFER_DEFAULT_HEIGHT
    wrap x | x == gl_FRAMEBUFFER_DEFAULT_WIDTH = DefaultWidth
           | x == gl_FRAMEBUFFER_DEFAULT_HEIGHT = DefaultHeight
           | otherwise = undefined

instance HasIntegerParam GLenum DrawFramebuffer where
    parami p fb = framebufferBasicParameteri gl_DRAW_FRAMEBUFFER fb p

{- Has parameters of type GLuint which are acessable by the data FramebufferParameter for
 - the type DrawFramebuffer -}
instance HasParamOfType GLuint FramebufferParameter DrawFramebuffer where
    param = parami . toGLEnum

instance IsGenerable DrawFramebuffer where
    generate = alloca $ \ptr -> do
        glGenFramebuffers 1 ptr
        liftM DrawFramebuffer $ peek ptr

instance IsBindable DrawFramebuffer where
    bind (DrawFramebuffer fb) = glBindFramebuffer gl_DRAW_FRAMEBUFFER fb

instance IsWrappedPrimitive GLuint DrawFramebuffer where
    unwrap (DrawFramebuffer fb) = fb
    wrap = DrawFramebuffer

instance IsFramebuffer DrawFramebuffer where
    getType _ = gl_DRAW_FRAMEBUFFER
