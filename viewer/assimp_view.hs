{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances #-}
{-https://github.com/assimp/assimp/blob/master/samples/SimpleOpenGL/Sample_SimpleOpenGL.c-}
module Main where

import Prelude hiding (catch)
import System.Environment (getArgs)
import Data.List (concatMap, foldl1')
import Control.Monad (forM_, when)
import Control.Applicative ((<$>))
import Control.Exception (catch, onException)
import Data.IORef
import Data.Ratio ((%))
import System.Exit (exitFailure)

import Data.Vect hiding (angle, normalize)
import Data.Vect.Float.OpenGL (multMatrix)
import Graphics.Formats.Assimp hiding (lookAt, Light, Diffuse)
import Graphics.UI.GLUT hiding (get, multMatrix)
import qualified Graphics.UI.GLUT as GLUT (get)
import Graphics.Rendering.OpenGL.GLU

class Coerce a b where
    coerce :: a -> b

instance Coerce Vec3 (Vector3 GLfloat) where
    coerce (Vec3 x y z) = Vector3 (rat x) (rat y) (rat z)
        where rat = fromRational . toRational

instance Coerce Color4F (Color4 GLfloat) where
    coerce (Color4F (Vec4 r g b a)) = rat `fmap` Color4 r g b a
        where rat = fromRational . toRational

data Context = Context
    { scene     :: Scene
    , center    :: Vec3
    , sceneMin  :: Vec3
    , sceneMax  :: Vec3
    , angle     :: Float
    , prevTime  :: Int
    , sceneList :: DisplayList
    , frames    :: Int
    } deriving (Show)

processing = 
    [ CalcTangentSpace
    , Triangulate
    , JoinIdenticalVertices
    , SortByPType
    ]

aiTransformVecByMatrix4 :: Vec3 -> Mat4 -> Vec3
aiTransformVecByMatrix4 (Vec3 x y z)
                  (Mat4 (Vec4 a1 a2 a3 a4)
                        (Vec4 b1 b2 b3 b4)
                        (Vec4 c1 c2 c3 c4)
                        (Vec4 d1 d2 d3 d4)) = Vec3 
    (a1 * x + a2 * y + a3 * z + a4)
    (b1 * x + b2 * y + b3 * z + b4)
    (c1 * x + c2 * y + c3 * z + c4)

getBoundingBox :: Scene -> Mat4 -> Node -> (Vec3, Vec3)
getBoundingBox scene trans node = (f min, f max)
    where
    trans' :: Mat4
    trans' = trans .*. transformation node
    nMeshes :: [Mesh]
    nMeshes = map (meshes scene !!) $ map fromIntegral $ nodeMeshes node
    verts, verts' :: [Vec3]
    verts  = concatMap vertices nMeshes
    verts' = map (`aiTransformVecByMatrix4` trans') verts
    compV :: (Float -> Float -> Float) -> Vec3 -> Vec3 -> Vec3
    compV comp (Vec3 x1 y1 z1) (Vec3 x2 y2 z2) = 
        Vec3 (x1 `comp` x2) (y1 `comp` y2) (z1 `comp` z2)
    childs :: [Vec3]
    childs = concatMap (\(x,y) -> [x,y]) $ map (getBoundingBox scene trans') $ children node
    f :: (Float -> Float -> Float) -> Vec3
    f comp = foldl1' (compV comp) $ verts' ++ childs

sceneCenter :: Scene -> Vec3
sceneCenter scene = 
    let (min, max) = getBoundingBox scene idmtx $ rootNode scene
    in (min &+ max) &* 2

applyMaterial :: Material -> IO ()
applyMaterial mat = do
    --get mat KeyColorDiffuse >>= \Right c -> materialDiffuse FrontAndBack  
    materialDiffuse  FrontAndBack $= Color4 0.8 0.8 0.8 1
    materialSpecular FrontAndBack $= Color4 0   0   0   1
    materialAmbient  FrontAndBack $= Color4 0.2 0.2 0.2 1
    materialEmission FrontAndBack $= Color4 0   0   0   1
  
    --ret1 <- get mat KeyShininess
    let ret1 = Left "err"
    case ret1 of
        Right shininess -> do
            ret2 <- get mat KeyShininessStrength
            let shininess' = (fromRational . toRational) (shininess :: Float) -- TODO(maybe Float is the wrong type)
            case ret2 of
                Right strength ->
                    materialShininess FrontAndBack $= shininess' * (fromRational $ toRational (strength::Float))
                Left _ ->
                    materialShininess FrontAndBack $= shininess'
        Left err -> do
            materialShininess FrontAndBack $= 0
            materialSpecular  FrontAndBack $= Color4 0 0 0 0
  
    --ret3 <- get mat KeyEnableWireframe :: IO (Either String Int)
    let ret3 = Right 1
    let mode = case ret3 of
                   Right x | x /= 0 -> Line
                   _                -> Fill
    polygonMode $= (mode, mode)
  
    --ret4 <- get mat KeyTwoSided :: IO (Either String Int)
    let ret4 = Left "err"
    cullFace $= case ret4 of
        Right x | x /= 0 -> Just FrontAndBack
        _                -> Nothing -- Just Front ?

render :: Scene -> Node -> IO ()
render scene node = flip onException (putStrLn "render error") $ preservingMatrix $ do
    let m = transpose $ transformation node
    multMatrix m

    forM_ [0..(length $ nodeMeshes node)-1] $ \n -> do
        let mesh = (!!n) . meshes $ scene
        --applyMaterial $ (!! materialIndex mesh) . materials $ scene
        applyMaterial $ materials scene !! (fromIntegral $ materialIndex mesh)

        lighting $= if length (normals mesh) > 0 then Enabled else Disabled

        forM_ (faces mesh) $ \face -> do
            let mode = case length $ indices face of
                    1 -> Points
                    2 -> Lines
                    3 -> Triangles
                    _ -> Polygon
            renderPrimitive mode $ forM_ (indices face) $ \index' -> do
                let index = fromIntegral index'
                when (length (colors mesh) > 0) $
                    color $ (coerce $ (colors mesh) !! index :: (Color4 GLfloat))
                when (length (normals mesh) > 0) $
                    normal $ (!!index) $ normals mesh
                vertex $ (!!index) $ vertices mesh

    forM_ (children node) $ \c ->
        render scene c

doMotion :: IORef Context -> IO ()
doMotion context = do
    time <- GLUT.get elapsedTime

    ctx <- readIORef context
    let prev_time = prevTime ctx
        angle'    = angle ctx + (fromIntegral $ time - prev_time)*0.01
    modifyIORef context $ \c -> c {angle = angle', prevTime = time}
    postRedisplay Nothing

display :: IORef Context -> DisplayCallback
display context = do
    ctx <- readIORef context
    clear [ColorBuffer, DepthBuffer]
    matrixMode $= Modelview 0 -- not sure which number
    loadIdentity
    lookAt (Vertex3 0 0 3) (Vertex3 0 0 (-5)) (Vector3 0 1 0)
    rotate (fromRational $ toRational $ angle ctx :: GLfloat) $ Vector3 0 1 0

    let Vec3 x1 y1 z1 = sceneMin ctx
        Vec3 x2 y2 z2 = sceneMax ctx
        tmp  = maximum [x2 - x1, y2 - y1, z2 - z1]
        tmp' = fromRational $ toRational $ 1 / tmp :: GLfloat
    scale tmp' tmp' tmp' -- not sure if this is right

    translate (coerce $ (&*(-1)) `fmap` sceneCenter $ scene ctx :: Vector3 GLfloat)

    let DisplayList sl = sceneList ctx
    when (sl == 0) $ do
        dl <- defineNewList Compile $ do
            let s = scene ctx
            render s (rootNode s)
        modifyIORef context $ \c -> c {sceneList = dl}

    callList $ sceneList ctx

    -- write fps
    loadIdentity
    color (Color3 0.3 0.3 1 :: Color3 GLfloat)
    translate (Vector3 0 0 (-5) :: Vector3 GLfloat)
    rasterPos (Vertex2 1 1 :: Vertex2 GLfloat)

    let fps = (fromIntegral $ (frames ctx) + 1) / (fromIntegral $ (prevTime ctx) + 1) * 1000 :: Float
    renderString Helvetica12 $ show fps ++ " fps"
    modifyIORef context $ \c -> c {frames = (frames ctx) + 1}

    swapBuffers
    doMotion context

reshape :: ReshapeCallback
reshape size@(Size w h) = do
    let aspectRatio = (fromIntegral w) / (fromIntegral h)
        fieldOfView = 45

    matrixMode $= Projection
    loadIdentity
    perspective fieldOfView aspectRatio 1 1000
    viewport $= (Position 0 0, size)

main = do
    args <- getArgs >>= initialize "Assimp viewer"
    when (length args /= 1) $ do
      putStrLn "usage: assimp_view <filename>"
      exitFailure
    let file = head args
    Right scene <- importFile file processing
    print $ materials scene
    let (min, max) = getBoundingBox scene idmtx $ rootNode scene
    ctx <- newIORef $ Context scene (sceneCenter scene) min max 0 0 (DisplayList 0) 0

    initialDisplayMode $= [DoubleBuffered, RGBMode, WithDepthBuffer]
    initialWindowSize $= Size 500 600
    initialWindowPosition $= Position 50 50
    createWindow "Assimp viewer"
    reshapeCallback $= Just reshape
    displayCallback $= display ctx

    clearColor        $= Color4 0.1 0.1 0.1 1
    lighting          $= Enabled
    light (Light 0)   $= Enabled
    depthFunc         $= Just Less
    lightModelTwoSide $= Enabled
    normalize         $= Enabled
    frontFace         $= CW
    colorMaterial     $= Just (FrontAndBack, Diffuse)

    mainLoop
