{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
module Graphics.Glyph.GLMath where
import Graphics.Glyph.Mat4
import qualified Graphics.Rendering.OpenGL as GL
import Graphics.Rendering.OpenGL (GLfloat,Uniform,Vertex3(..),uniform,UniformComponent)
import Data.Angle
import Data.Maybe
import Debug.Trace

data Vec2 a = Vec2 (a,a) deriving (Show,Eq)
data Vec3 a = Vec3 (a,a,a) deriving (Show,Eq)
data Vec4 a = Vec4 (a,a,a,a) deriving (Show,Eq)

instance UniformComponent a => Uniform (Vec3 a) where
    uniform loc = GL.makeStateVar
                    (do
                        (Vertex3 x y z) <-
                            GL.get (uniform loc)
                        return (Vec3 (x,y,z)) )
                    (\(Vec3 (x,y,z)) -> uniform loc GL.$= Vertex3 x y z)
    uniformv _ = undefined

instance UniformComponent a => Uniform (Vec4 a) where
    uniform loc = GL.makeStateVar
                    (do
                        (GL.Vertex4 x y z w) <-
                            GL.get (uniform loc)
                        return (Vec4 (x,y,z,w)) )
                    (\(Vec4 (x,y,z,w)) -> uniform loc GL.$= GL.Vertex4 x y z w)
    uniformv _ = undefined

class (Floating flT) => Vector flT b where
    (<+>) :: b flT -> b flT -> b flT
    (<->) :: b flT -> b flT -> b flT
    norm :: b flT -> flT
    normalize :: b flT -> b flT
    vDot :: b flT -> b flT -> flT
    vScale :: flT -> b flT -> b flT
    vNegate :: b flT -> b flT


(<.>) :: (Vector a b) => b a -> b a -> a
(<.>) = vDot

(|||) :: (Vector a b) => b a -> a
(|||) = norm

instance (Floating flT) => Vector flT Vec2 where
    (<+>) (Vec2 (a,b)) (Vec2 (c,d)) = Vec2 (a+c,b+d)
    (<->) (Vec2 (a,b)) (Vec2 (c,d)) = Vec2 (a-c,b-d)
    vDot (Vec2 (a,b)) (Vec2 (c,d)) = a * c + b * d
    vScale c (Vec2 (a,b)) = Vec2 (a*c,b*c)
    norm (Vec2 (a,b)) = sqrt (a*a + b*b)
    normalize vec@(Vec2 (a,b)) = 
        let n = norm vec in Vec2 (a/n,b/n)
    vNegate (Vec2 (a,b)) = Vec2 (-a,-b)

instance (Floating flT) => Vector flT Vec3 where
    (<+>) (Vec3 (a,b,c)) (Vec3 (d,e,f)) = Vec3 (a+d,b+e,c+f)
    (<->) (Vec3 (a,b,c)) (Vec3 (d,e,f)) = Vec3 (a-d,b-e,c-f)
    vDot (Vec3 (a,b,c)) (Vec3 (d,e,f)) = a * d + b * e + c * f
    vScale x (Vec3 (a,b,c)) = Vec3 (a*x,b*x,c*x)
    norm (Vec3 (a,b,c)) = sqrt (a*a + b*b + c*c)
    normalize vec@(Vec3 (a,b,c)) = 
        let n = norm vec in Vec3 (a/n,b/n,c/n)
    vNegate (Vec3 (a,b,c)) = Vec3 (-a,-b,-c)

instance (Floating flT) => Vector flT Vec4 where
    (<+>) (Vec4 (a,b,c,g)) (Vec4 (d,e,f,h)) = Vec4 (a+d,b+e,c+f,g+h)
    (<->) (Vec4 (a,b,c,g)) (Vec4 (d,e,f,h)) = Vec4 (a-d,b-e,c-f,g-h)
    vDot  (Vec4 (a,b,c,g)) (Vec4 (d,e,f,h)) = a * d + b * e + c * f + g * h
    vScale x (Vec4 (a,b,c,d)) = Vec4 (a*x,b*x,c*x,d*x)
    norm (Vec4 (a,b,c,d)) = sqrt (a*a + b*b + c*c + d*d)
    normalize vec@(Vec4 (a,b,c,d)) = 
        let n = norm vec in Vec4 (a/n,b/n,c/n,d/n)
    vNegate (Vec4 (a,b,c,d)) = Vec4 (-a,-b,-c,-d)

cross :: (Num a) => Vec3 a -> Vec3 a -> Vec3 a
cross (Vec3 (u1,u2,u3)) (Vec3 (v1,v2,v3)) =
        Vec3 ( u2*v3 - u3*v2,
                  u3*v1 - u1*v3,
                  u1*v2 - u2*v1 )
(×) :: (Num a) => Vec3 a -> Vec3 a -> Vec3 a
(×) = cross

lookAtMatrix :: Vec3 GLfloat -> Vec3 GLfloat -> Vec3 GLfloat -> Mat4 GLfloat
lookAtMatrix e c u =
    let f@(Vec3 (fx,fy,fz)) = normalize (c <-> e)
        s@(Vec3 (sx,sy,sz)) = normalize (f × u)
        u'@(Vec3 (ux,uy,uz))   = s × f in 
           Matrix4 (sx, ux, -fx, 0,
                   sy, uy, -fy, 0,
                   sz, uz, -fz, 0,
                   -(s<.>e) , -(u'<.>e),  f<.>e,  1 )

orthoMatrix :: GLfloat -> GLfloat -> GLfloat -> GLfloat -> GLfloat -> GLfloat -> Mat4 GLfloat
orthoMatrix top bot right left near far =
    Matrix4 (2 / (right-left), 0, 0, - (right + left) / (right - left),
             0, 2 / (top-bot), 0, - (top+bot) / (top-bot),
             0, 0, -2 / (far-near), - (far+near) / (far - near),
             0, 0, 0, 1)
perspectiveMatrix :: GLfloat -> GLfloat -> GLfloat -> GLfloat -> Mat4 GLfloat
{- as close to copied from glm as possible -}
perspectiveMatrix fov asp zn zf =
    let tanHalfFovy = tangent (Degrees fov/2)
        res00 = 1 / (asp * tanHalfFovy)
        res11 = 1 / tanHalfFovy
        res22 = - (zf + zn) / (zf - zn)
        res23 = - 1
        res32 = - (2 * zf * zn) / (zf - zn) in
    trace ("res22=" ++ show res22) $
    Matrix4 (res00, 0, 0, 0,
            0, res11, 0, 0,
            0, 0, res22, res23,
            0, 0, res32, 0)

class VectorMatrix vecT matT where
    vTranslate :: matT -> vecT -> matT
    (-*|)  :: matT -> vecT -> vecT

instance (Num a) => VectorMatrix (Vec3 a) (Mat3 a) where
    vTranslate (Matrix3 (a00,a01,a02,
                         a10,a11,a12,
                         a20,a21,a22)) (Vec3 (a,b,c)) =
                    Matrix3 (a00,a01,a02+a,
                             a10,a11,a12+b,
                             a20,a21,a22+c)

    (Matrix3 (a00,a01,a02,
             a10,a11,a12,
             a20,a21,a22)) -*| (Vec3 (a,b,c)) = 
                                Vec3 (a00 * a + a01 * b + a02 * c,
                                      a10 * a + a11 * b + a12 * c,
                                      a20 * a + a21 * b + a22 * c )




instance (Num a) => VectorMatrix (Vec4 a) (Mat4 a) where
    vTranslate mat (Vec4 tmp) = translateMat4 mat tmp
    mat -*| tmp = glslMatMul mat tmp

glslMatMul :: (Num a) => Mat4 a -> Vec4 a -> Vec4 a
glslMatMul (Matrix4 (m00,m01,m02,m03,
            m10,m11,m12,m13,
            m20,m21,m22,m23,
            m30,m31,m32,m33)) (Vec4 (v0,v1,v2,v3)) =
                Vec4 ( v0 * m00 + v1 * m10 + v2 * m20 + v3 * m30,
                       v0 * m01 + v1 * m11 + v2 * m21 + v3 * m31,
                       v0 * m02 + v1 * m12 + v2 * m22 + v3 * m32,
                       v0 * m03 + v1 * m13 + v2 * m23 + v3 * m33 )

glslModelViewToNormalMatrix :: Mat4 GLfloat -> Mat3 GLfloat
glslModelViewToNormalMatrix = fromJust.inverse.transpose.trunc4

(==>) :: (Num a) => Mat4 a -> Vec4 a -> Mat4 a
(==>) = glslMatTranslate
glslMatTranslate :: (Num a) => Mat4 a -> Vec4 a -> Mat4 a
glslMatTranslate
    mat@(Matrix4 (m00,m01,m02,m03,
            m10,m11,m12,m13,
            m20,m21,m22,m23,
            m30,m31,m32,m33)) vec =
                let (Vec4 (v0,v1,v2,v3)) = mat -*| vec in
                Matrix4 (m00,m01,m02,m03,
                         m10,m11,m12,m13,
                         m20,m21,m22,m23,
                         m30+v0,m31+v1,m32+v2,m33+v3)
                
rotationMatrix :: GLfloat -> Vec3 GLfloat -> Mat3 GLfloat
rotationMatrix ang (Vec3 (u,v,w)) =
    let l = (u*u + v*v + w*w)
        u2 = u*u
        v2 = v*v
        w2 = w*w in
        Matrix3 (
            (u2 + (v2 + w2) * cos(ang)) / l,
            (u * v * (1 - cos(ang)) - w * sqrt(l) * sin(ang)) / l,
            (u * w * (1 - cos(ang)) + v * sqrt(l) * sin(ang)) / l,

            (u * v * (1 - cos(ang)) + w * sqrt(l) * sin(ang)) / l,
            (v2 + (u2 + w2) * cos(ang)) / l,
            (v * w * (1 - cos(ang)) - u * sqrt(l) * sin(ang)) / l,

            (u * w * (1 - cos(ang)) - v * sqrt(l) * sin(ang)) / l,
            (v * w * (1 - cos(ang)) + u * sqrt(l) * sin(ang)) / l,
            (w2 + (u2 + v2) * cos(ang)) / l
        )

zRotationMatrix :: GLfloat -> Mat3 GLfloat
zRotationMatrix ang = rotationMatrix ang (Vec3 (0,0,1))

rotateFrom :: Vec3 GLfloat -> Vec3 GLfloat -> Vec3 GLfloat -> Vec3 GLfloat
rotateFrom vector relative newRelative =
    if vector == Vec3 (0,0,0) then vector else
        case (normalize relative, normalize newRelative) of
            (r', n') ->
                if r' == n' then vector else
                    let axis = r' × n'
                        ang  = acos $ r' `vDot` n' in
                        rotationMatrix ang axis -*| vector
                
