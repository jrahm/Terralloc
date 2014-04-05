module Graphics.Glyph.ExtendedGL where

import Graphics.Rendering.OpenGL
import Graphics.Rendering.OpenGL.Raw.Core31
import Graphics.Rendering.OpenGL.Raw.ARB

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
