module Models where

import Graphics.Glyph.GeometryBuilder 
import Graphics.Glyph.BufferBuilder

import Control.Monad
import Data.ByteString.Lazy

import Graphics.Glyph.GLMath
import Graphics.Glyph.ObjLoader

square :: (Num b,IsModelBuilder b a) => b -> a ()
square dist = do
    plotVertex3   dist    dist  0
    plotVertex3 (-dist)   dist  0
    plotVertex3 (-dist) (-dist)  0
    plotVertex3   dist  (-dist)  0

getBS :: GeometryBuilder () -> ByteString
getBS = buildSource

getAsStr :: GeometryBuilder () -> String
getAsStr = buildSourceAsString

treeShader :: ByteString
treeShader = buildSource tree

triangle :: GeometryBuilder ()
triangle =
    generating Triangles $ do
        projectionMatrixUniform "pjMatrix"
        modelViewMatrixUniform "mvMatrix"
        textureOutput "texposition"
        normalOutput "normal"
        positionOutput "frag_position"
    
        gVertex4E 1 0 0 0
        gVertex4E 0 1 0 0
        gVertex4E 0 0 1 0

tree :: GeometryBuilder ()
tree =
    generating TriangleStrip $ do
        projectionMatrixUniform "pjMatrix"
        modelViewMatrixUniform "mvMatrix"
        textureOutput "texposition"
        normalOutput "normal"
        positionOutput "frag_position"
    
        let r = 0.045
        let h = 0.4
    
    
        forM_ [0..6.4] $ \th -> do
            let vertex x y z = do
                gNormal3 x 0 z
                gVertex4E x y z 0
    
            let c = r * cos th
            let s = r * sin th
    
            let c2 = r * (cos $ th + 1.0)
            let s2 = r * (sin $ th + 1.0)
    
            let texX = th / 6.4 / 2.0
            let texX2 = (th+1.0) / 6.4 / 2.0
    
            let quads = trianglesFromQuads
                    [(gTexture2 texX  0 >> vertex c 0 s),
                    (gTexture2 texX  1 >> vertex c  h s),
                    (gTexture2 texX2 1 >> vertex c2 h s2),
                    (gTexture2 texX2 0 >> vertex c2 0 s2)]
    
            sequence_ quads
    
        forM_ [0..6.4] $ \th -> do
            let vertex x y z = do
                gNormal3 x 0 z
                gVertex4E x y z 0
    
            let c = r * 4 * cos th
            let s = r * 4 * sin th
            let texX = th / 6.4 / 2.0 + 0.5
    
            gTexture2 texX 1
            vertex 0 (h*2) 0
            gTexture2 texX 0
            vertex s (h/4) c
