{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Graphics.Glyph.GeometryBuilder where

import Data.Sequence as Seq
import Data.Setters
import Data.Maybe

import Graphics.Glyph.Util
import Graphics.Glyph.BufferBuilder

import Data.ByteStringBuilder
import Data.ByteString.Lazy
import Data.ByteString.Lazy.Char8 as BSLC
import Data.Foldable as Fold

import Text.Printf

data OutType = TriangleStrip | Triangles
instance Show OutType where
    show TriangleStrip = "triangle_strip"
    show Triangles = "triangle_strip"

buildSourceAsString :: GeometryBuilder a -> String
buildSourceAsString = BSLC.unpack . buildSource

buildSource :: GeometryBuilder a -> ByteString
buildSource builder =
    runBuilder $ do
        putSLn "#version 150"
        putSLn "#extension GL_ARB_explicit_attrib_location : enable"
        putSLn "#extension GL_ARB_explicit_uniform_location : enable"
        putSLn "layout(points) in ;"
    
        let isVertex (Vertex _ _ _ _) = True
            isVertex _ = False
        putSLn $ printf "layout(%s,max_vertices=%d) out ;"
                    (show $ maybeDefault TriangleStrip $ gOutType builder)
                    (Seq.length $ Seq.filter isVertex $ gList builder)

        forM_ (textureOut builder) $ putSLn.("out vec2 "++) . (++";")
        forM_ (normalOut builder) $ putSLn.("out vec3 "++) . (++";")
        forM_ (positionOut builder) $ putSLn.("out vec4 "++) . (++";")
        
        let pjMatStr = fromJust (pjMatrixUniform builder >||> Just "pjMatrix")
        let mvMatStr = fromJust (mvMatrixUniform builder >||> Just "mvMatrix")

        Fold.mapM_ (putSLn.("uniform mat4 "++).(++";")) [pjMatStr, mvMatStr]

        putSLn "void main() {"

        let vertexOutF =
             case positionOut builder of
                Nothing ->
                    printf "\tgl_Position = %s * (gl_in[0].gl_Position + %s * vec4(%f,%f,%f,%f));"
                        pjMatStr mvMatStr
                Just str -> 
                    printf "\tgl_Position = %s * (%s = gl_in[0].gl_Position + %s * vec4(%f,%f,%f,%f));"
                        pjMatStr str mvMatStr
        let normalOutF = case normalOut builder of
             Nothing -> const3 ""
             Just str -> printf "\t%s = -inverse(transpose(mat3(%s))) * vec3(%f,%f,%f);" str mvMatStr

        let textureOutF = case textureOut builder of
             Nothing -> const2 ""
             Just str -> printf "\t%s = vec2(%f,%f);" str

        forM_ (gList builder) $ \datum ->
            case datum of
                Vertex x y z w -> putSLn $ vertexOutF x y z w
                Normal x y z -> putSLn $ normalOutF x y z
                Texture x y -> putSLn $ textureOutF x y
                EmitVertex -> putSLn "\tEmitVertex();"
                EndPrimitive -> putSLn "\tEndPrimitive();"
        putSLn "}"

data GeometryDatum =
    Vertex  Float Float Float Float |
    Texture Float Float |
    Normal  Float Float Float |
    EmitVertex |
    EndPrimitive

data GeometryBuilder a = GeometryBuilder {
    gList :: (Seq GeometryDatum),

    gOutType :: Maybe OutType,
    pjMatrixUniform :: Maybe String,
    mvMatrixUniform :: Maybe String,
    maxVerts :: Maybe Int,

    textureOut :: Maybe String,
    normalOut :: Maybe String,
    positionOut :: Maybe String,
    gRet :: a
}

$(declareSetters ''GeometryBuilder)

generating :: OutType -> GeometryBuilder () -> GeometryBuilder ()
generating TriangleStrip builder = setGOutType (Just TriangleStrip) $ builder
generating Triangles builder = do
    let (nSeq,_) =
         Fold.foldl' (\(tSeq,cnt) datum ->
            case datum of
                EmitVertex ->
                    if cnt == (2::Int) then (tSeq |> datum |> EndPrimitive, 0)
                    else (tSeq |> datum, cnt + 1)
                _ -> (tSeq |> datum,cnt)
            ) (Seq.empty, 0) (gList builder)

    setGOutType (Just Triangles) $
        setGList nSeq builder

projectionMatrixUniform :: String -> GeometryBuilder ()
projectionMatrixUniform str = setPjMatrixUniform (Just str) $ return ()

modelViewMatrixUniform :: String -> GeometryBuilder ()
modelViewMatrixUniform str = setMvMatrixUniform (Just str) $ return ()

maxVerticies :: Int -> GeometryBuilder ()
maxVerticies i = setMaxVerts (Just i) $ return ()

textureOutput :: String -> GeometryBuilder ()
textureOutput str = setTextureOut (Just str) $ return ()

normalOutput :: String -> GeometryBuilder ()
normalOutput str = setNormalOut (Just str) $ return ()

positionOutput :: String -> GeometryBuilder ()
positionOutput str = setPositionOut (Just str) $ return ()

gVertex4 :: Float -> Float -> Float -> Float -> GeometryBuilder ()
gVertex4 x y z w = setGList (Seq.singleton $ Vertex x y z w) $ return ()

gNormal3 :: Float -> Float -> Float -> GeometryBuilder ()
gNormal3 x y z = setGList (Seq.singleton $ Normal x y z) $ return ()

gTexture2 :: Float -> Float -> GeometryBuilder ()
gTexture2 x y = setGList (Seq.singleton $ Texture x y) $ return ()

gEmitVertex :: GeometryBuilder ()
gEmitVertex = setGList (Seq.singleton $ EmitVertex) $ return ()

gEndPrimitive :: GeometryBuilder ()
gEndPrimitive = setGList (Seq.singleton $ EndPrimitive) $ return ()

gVertex4E :: Float -> Float -> Float -> Float -> GeometryBuilder ()
gVertex4E x y z w = gVertex4 x y z w >> gEmitVertex


instance Monad GeometryBuilder where
    aB >> bB = GeometryBuilder
                (gList aB >< gList bB)
                (select gOutType gOutType)
                (select pjMatrixUniform pjMatrixUniform)
                (select mvMatrixUniform mvMatrixUniform)
                (select maxVerts maxVerts)
                (select textureOut textureOut)
                (select normalOut normalOut)
                (select positionOut positionOut)
                (gRet bB)
                where select f1 f2 = (f1 bB) >||> (f2 aB)
    aB >>= func = aB >> func (gRet aB)
    return = GeometryBuilder
                Seq.empty
                Nothing
                Nothing
                Nothing
                Nothing
                Nothing
                Nothing
                Nothing
    fail = error
    

instance IsModelBuilder Float GeometryBuilder where
    plotVertex3 x y z = gVertex4E x y z 0
    plotNormal = gNormal3
    plotTexture = gTexture2

