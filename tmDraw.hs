import Graphics.Rendering.OpenGL
import qualified Graphics.UI.GLUT as GLUT
import Data.IORef
import Data.List
import qualified Data.Map as Map
import Data.Map (Map)
import qualified Data.Set as Set
import Data.Set (Set)
import qualified Control.Monad.Random as Random
import Debug.Trace
import Control.Concurrent (threadDelay)

data Camera = Camera { camera_r :: Float
                     , camera_theta :: Float
                     , camera_phi :: Float
                     }

type VertexIndex = Int
type Mesh = ([Vertex3 GLfloat], [[VertexIndex]])

data State = State { state_config :: Config
                   , state_camera :: Camera
                   , state_t :: Int
                   , state_globe :: Mesh
                   , state_globe_map :: Map VertexIndex (Vertex3 GLfloat)
                   , state_position :: VertexIndex
                   , state_tm :: TM
                   }

data TapeColor = ColorBlank | ColorRed | ColorGreen | ColorBlue deriving (Show,Eq)
type Q = Int
data Tape = Tape (Map VertexIndex (TapeColor, [VertexIndex])) deriving Show
data Config = Config (VertexIndex, VertexIndex) Tape Q
type Direction = Int
data TM = TM [((Q, TapeColor), (Q, TapeColor, Direction))] deriving Show

removeDups :: Ord a => [a] -> Set a -> [a]
removeDups [] sofar = []
removeDups (x:rest) sofar
     | Set.member x sofar = (removeDups rest sofar)
     | otherwise          = x:(removeDups rest (Set.insert x sofar))

neighbors :: VertexIndex -> [[VertexIndex]] -> [VertexIndex]
neighbors i = (\ns -> removeDups ns Set.empty) . concat . map (filter (/= i)) . filter (i `elem`)

initTape :: Mesh -> Tape
initTape (vs, fs) = foldl' (f (vs, fs)) (Tape $ Map.empty) [0..length vs-1]
    where f :: ([Vertex3 GLfloat], [[VertexIndex]]) -> Tape -> VertexIndex -> Tape
          f (vs, fs) (Tape tape) i = let p = vs !! i
                                         ns = neighbors i fs
                                         ns' = orderNeighbors p (zip ns (map (vs !!) ns))
                                     in Tape $ Map.insert i (ColorBlank, (ns')) tape

          {-angle = undefined

          order :: Vertex3 GLfloat -> Vertex3 GLfloat -> Vertex3 GLfloat -> Vertex3 GLfloat -> Ordering
          order p@(Vertex3 xp yp zp) (Vertex3 xr yr zr) (Vertex3 x1 y1 z1) (Vertex3 x2 y2 z2) =
             let p_to_r = Vertex3 (xr-xp) (yr-yp) (zr-zp)
                 p_to_x1 = Vertex3 (x1-xp) (y1-yp) (z1-zp)
                 p_to_x2 = Vertex3 (x2-xp) (y2-yp) (z2-zp)
             in compare (angle p p_to_r p_to_x1) (angle p p_to_r p_to_x2)-}

          orderNeighbors :: Vertex3 GLfloat -> [(VertexIndex, Vertex3 GLfloat)] -> [VertexIndex]
          orderNeighbors _ ns = map fst ns
          --orderNeighbors p ns@((_,r):_) = map fst $ sortBy (\(i1, p1) (i2, p2) -> order p r p1 p2) ns

randomTM :: Random.RandomGen g => Q -> Random.Rand g TM
randomTM numQ = do trans <- mapM (\(q, c) -> do q' <- Random.getRandomR (0, numQ-1)
                                                c' <- randomColor
                                                d  <- Random.getRandomR (0,5)
                                                return ((q, c), (q', c', d)))
                                 [(q, c) | q <- [0..numQ-1], c <- [ColorBlank, ColorRed, ColorGreen, ColorBlue]]
                   return (TM trans)

    where 
      randomColor ::  Random.RandomGen g => Random.Rand g TapeColor
      randomColor = do i <- Random.getRandomR (0, 2) 
                       return $ case i :: Int of
                                  0 -> ColorRed
                                  1 -> ColorGreen
                                  2 -> ColorBlue


step :: TM -> Config -> Config
step (TM trans) (Config (oldId, curId) (Tape tape) q) = let Just (c, ns) = Map.lookup curId tape
                                                            Just (q', c', dir) = Data.List.lookup (q,c) trans
                                                            tape' = Tape $ Map.insert curId (c', ns) tape
                                                            Just refIdx = Data.List.elemIndex oldId ns
                                                            newId = ns !! ((refIdx + dir) `mod` 6)
                                                        in  Config (curId, newId) tape' q'

normalizeVertex :: Vertex3 GLfloat -> Vertex3 GLfloat
normalizeVertex (Vertex3 x y z) = let r = sqrt $ x^^2 + y^^2 + z^^2
                                  in Vertex3 (x / r) (y / r) (z / r)
                     
icosahedron_v :: [Vertex3 GLfloat]
icosahedron_v = let t = (1.0 + sqrt(5.0)) / 2.0
                in [ normalizeVertex $ Vertex3 (-1)  t  0
                   , normalizeVertex $ Vertex3  1  t  0
                   , normalizeVertex $ Vertex3 (-1) (-t)  0
                   , normalizeVertex $ Vertex3  1 (-t)  0
                  
                   , normalizeVertex $ Vertex3  0 (-1)  t
                   , normalizeVertex $ Vertex3  0  1  t
                   , normalizeVertex $ Vertex3  0 (-1) (-t)
                   , normalizeVertex $ Vertex3  0  1 (-t)
                  
                   , normalizeVertex $ Vertex3  t  0 (-1)
                   , normalizeVertex $ Vertex3  t  0  1
                   , normalizeVertex $ Vertex3 (-t)  0 (-1)
                   , normalizeVertex $ Vertex3 (-t)  0  1
                   ]
                    
icosahedron_color :: [Color4 GLfloat]
icosahedron_color = [ Color4 1.0 0.0 0.0 1.0
                    , Color4 0.0 1.0 0.0 1.0
                    , Color4 0.0 0.0 1.0 1.0
                    , Color4 0.5 0.5 1.0 1.0
                     
                    , Color4 0.0 0.5 0.0 1.0
                    , Color4 0.5 0.0 0.5 1.0
                    , Color4 0.5 1.0 0.0 1.0
                    , Color4 0.8 0.8 0.8 1.0
                     
                    , Color4 1.0 0.0 1.0 1.0
                    , Color4 0.2 0.4 0.6 1.0
                    , Color4 8.8 1.0 99.0 1.0
                    , Color4 0.2 0.2 0.2 1.0
                    ]
                    
icosahedron_faces :: [[VertexIndex]]
icosahedron_faces = [[0,11,5], [0,5,1], [0,1,7], [0,7,10], [0,10,11],
                     [1,5,9], [5,11,4], [11,10,2], [10,7,6], [7,1,8],
                     [3,9,4], [3,4,2], [3,2,6], [3,6,8], [3,8,9],
                     [4,9,5], [2,4,11], [6,2,10], [8,6,7], [9,8,1]]

icosahedron :: Mesh
icosahedron = (icosahedron_v, icosahedron_faces)

type SplitCache = [((VertexIndex, VertexIndex), VertexIndex)]
          
split :: Mesh -> Mesh
split (vs, fs) = let (mesh, _) = foldl' f ((vs, []), []) fs
                 in mesh
    where f :: (Mesh, SplitCache) -> [VertexIndex] -> (Mesh, SplitCache)
          f ((vs, fs), sc) [i1,i2,i3] = let (a, (vs',   sc'))   = get_middle i1 i2 (vs, sc)
                                            (b, (vs'',  sc''))  = get_middle i2 i3 (vs', sc')
                                            (c, (vs''', sc''')) = get_middle i3 i1 (vs'', sc'')
                                            
                                            f1 = [i1, a, c]
                                            f2 = [i2, b, a]
                                            f3 = [i3, c, b]
                                            f4 = [a, b, c]
                                        in ((vs''', f1:f2:f3:f4:fs), sc''')
                                            
          get_middle :: VertexIndex -> VertexIndex -> ([Vertex3 GLfloat], SplitCache) -> (VertexIndex, ([Vertex3 GLfloat], SplitCache))
          get_middle i1 i2 mesh@(vs, sc) = case lookup (min i1 i2, max i1 i2) sc of
                                               Just i -> (i, mesh)
                                               Nothing -> let m = calc_middle (vs !! i1) (vs !! i2)
                                                              vs' = vs ++ [m]
                                                              i = length vs
                                                              sc' = ((min i1 i2, max i1 i2), i) : sc
                                                          in (i, (vs', sc'))
                                                        
          calc_middle :: Vertex3 GLfloat -> Vertex3 GLfloat -> Vertex3 GLfloat
          calc_middle (Vertex3 x1 y1 z1) (Vertex3 x2 y2 z2) = let xm  = ((x1+x2)*0.5)
                                                                  ym  = ((y1+y2)*0.5)
                                                                  zm  = ((z1+z2)*0.5)
                                                              in normalizeVertex $ Vertex3 xm ym zm
               
drawSphere :: IORef State -> IO ()
drawSphere state =  do (vertices, faces) <- state_globe `fmap` readIORef state
                       globe_map <- state_globe_map `fmap` readIORef state
                       Config p (Tape tape) _ <- state_config `fmap` readIORef state

                       mapM_ (\[i0, i1, i2] -> do
                       renderPrimitive Triangles $ do
                            let (Just v0) = Map.lookup i0 globe_map
                            let (Just v1) = Map.lookup i1 globe_map
                            let (Just v2) = Map.lookup i2 globe_map
                            let (Just c0) = c `fmap` fst `fmap` Map.lookup i0 tape
                            let (Just c1) = c `fmap` fst `fmap` Map.lookup i1 tape
                            let (Just c2) = c `fmap` fst `fmap` Map.lookup i2 tape
                            
                            --let (Just c0) = ctest `fmap` Map.lookup 

                            color c0
                            vertex v0
                            color c1
                            vertex v1
                            color c2
                            vertex v2) faces
    where --c :: Int -> Color4 GLfloat
          --c i = Color4 (realToFrac $ sin (fromIntegral i)) (realToFrac $ (fromIntegral i/1005.0)) (realToFrac $ (fromIntegral i)/500.0) 1.0
          --c i = Color4 (realToFrac $ fromIntegral i) (realToFrac $ fromIntegral i)  (realToFrac $ fromIntegral i) 1.0
          
          c :: TapeColor -> Color4 GLfloat
          c ColorBlank = Color4 0 0 0 1
          c ColorRed   = Color4 1 0 0 1
          c ColorGreen = Color4 0 1 0 1
          c ColorBlue  = Color4 0 0 1 1
          
          --ctest :: VertexIndex -> [VertexIndex] -> Color4 GLfloat
          --ctest p ns = let Just idx = List.elemIndex p ns
          --                 x = fromRational idx / 5.0
          --             in Color4 x x x 1.0
 
calcCamPos :: Camera -> Vertex3 GLdouble
calcCamPos Camera { camera_r = r , camera_theta = theta , camera_phi = phi } = Vertex3 (realToFrac $ r*(sin theta)*(cos phi)) (realToFrac $ r*(sin theta)*(sin phi)) (realToFrac $ r*(cos theta))
 
setupDisplay :: IORef State -> GLUT.DisplayCallback
setupDisplay state = do
    (vertices, _) <- state_globe `fmap` readIORef state
    Config (_, p) _ q <- state_config `fmap` readIORef state
    --print q
    i <- state_t `fmap` readIORef state
    let Vertex3 px py pz = vertices !! p
    let dist = 3
    let camPos = Vertex3 (realToFrac $ px * dist) (realToFrac $ py * dist) (realToFrac $ pz * dist) :: Vertex3 GLdouble
    
    matrixMode $= Modelview 0
    loadIdentity
    
    camera <- state_camera `fmap` readIORef state
    let cameraPos = calcCamPos camera
    lookAt (cameraPos) (Vertex3 0.0 0.0 0.0) (Vector3 0.0 1.0 0.0)
    --lookAt camPos (Vertex3 0.0 0.0 0.0) (Vector3 0.0 1.0 0.0)
  
    translate ((Vector3 0.0 0.0 (-0.0))::Vector3 GLfloat)
    --print i
    --rotate 60.0    ((Vector3 1.0 0.0 0.0)::Vector3 GLfloat)
    --rotate (-20 + fromIntegral i) ((Vector3 0.0 0.0 1.0)::Vector3 GLfloat)
    --lookAt pos look (Vector3 0.0 1.0 0.0)

    --translate ((Vector3 0.0 0.0 (-1.0))::Vector3 GLfloat)
    --rotate 60    ((Vector3 1.0 0.0 0.0)::Vector3 GLfloat)
    --rotate (-20) ((Vector3 0.0 0.0 1.0)::Vector3 GLfloat)
    
keyboardMouse :: IORef State -> GLUT.Key -> GLUT.KeyState -> GLUT.Modifiers -> GLUT.Position -> IO ()
keyboardMouse state (GLUT.Char 'r') GLUT.Down _ _ = do
    tm <- Random.evalRandIO (randomTM 3)
    print tm
    modifyIORef state (\state -> state { state_tm = tm })
keyboardMouse state (GLUT.Char 't') GLUT.Down _ _ = do
    tm <- Random.evalRandIO (randomTM 3)
    print tm
    modifyIORef state (\state -> state { state_tm = tm })
    Config x (Tape oldTape) x' <- state_config `fmap` readIORef state
    let blankedTape = Map.map (\(_,xs) -> (ColorBlank,xs)) oldTape
    modifyIORef state (\state -> state { state_config = Config x (Tape blankedTape) x' })
keyboardMouse state (GLUT.Char 'w') GLUT.Down _ _ = do
    camera <- state_camera `fmap` readIORef state
    let Camera { camera_r = r , camera_theta = theta , camera_phi = phi } = camera
    modifyIORef state (\state -> state { state_camera = Camera { camera_r = r , camera_theta = theta + 0.05, camera_phi = phi }})
keyboardMouse state (GLUT.Char 's') GLUT.Down _ _ = do
    camera <- state_camera `fmap` readIORef state
    let Camera { camera_r = r , camera_theta = theta , camera_phi = phi } = camera
    modifyIORef state (\state -> state { state_camera = Camera { camera_r = r , camera_theta = theta , camera_phi = phi + 0.05}})
keyboardMouse _ _ _ _ _ = return ()

display :: IORef State -> GLUT.DisplayCallback
display state = do
  clear [ColorBuffer, DepthBuffer]
  setupDisplay state
  drawSphere state
  GLUT.swapBuffers
  
idle :: IORef State -> GLUT.IdleCallback
idle state = do
    modifyIORef state (\state -> state { state_t = state_t state + 1 })
    modifyIORef state (\state -> state { state_config = last (take 100 (iterate (step (state_tm state)) (state_config state))) })
    --threadDelay $ 100000
    GLUT.postRedisplay Nothing
    return ()
 
lightDiffuse :: Color4 GLfloat
lightDiffuse = Color4 1.0 0.0 0.0 1.0
 
lightPosition :: Vertex4 GLfloat
lightPosition = Vertex4 1.0 1.0 1.0 0.0
 
initfn :: IO ()
initfn = let light0 = Light 0 
         in do --diffuse light0 $= lightDiffuse
               --position light0 $= lightPosition
               --light light0 $= Enabled
               --lighting $= Enabled
               
               polygonMode $= (Line, Line)
 
               depthFunc $= Just Lequal
 
               matrixMode $= Projection
               perspective 40.0 1.0 1.0 10.0
               
               matrixMode $= Modelview 0
               lookAt (Vertex3 0.0 0.0 5.0) (Vertex3 0.0 0.0 0.0) (Vector3 0.0 1.0 0.0)
  
               translate ((Vector3 0.0 0.0 (-1.0))::Vector3 GLfloat)
               rotate 60    ((Vector3 1.0 0.0 0.0)::Vector3 GLfloat)
               rotate (-20) ((Vector3 0.0 0.0 1.0)::Vector3 GLfloat)

               clearColor $= Color4 0.3 0.3 0.3 1.0
 
main :: IO ()
main = do
  GLUT.getArgsAndInitialize
  GLUT.initialDisplayMode $= [GLUT.DoubleBuffered, GLUT.RGBMode, GLUT.WithDepthBuffer]
  GLUT.createWindow "red 3D lighted cube"
  GLUT.windowSize $= Size 1280 1024

  tm <- Random.evalRandIO (randomTM 3)
  print tm

  let myTm = TM [ ((q,c),(q,ColorGreen,0))  | q <- [0..2], c <- [ColorBlank,ColorRed,ColorGreen,ColorBlue]]

  let globe@(globe_vs, globe_fs) = (iterate split icosahedron) !! 5
      tape = initTape globe
      globe_map = Map.fromList (zip [0..] globe_vs)
      firstSucc = head $ neighbors 0 globe_fs
  print $ neighbors 0 globe_fs

  state <- newIORef $ State { state_config = Config (0, firstSucc) tape 0
                            , state_camera = Camera { camera_r = 5
                                                    , camera_theta = 0
                                                    , camera_phi = 0
                                                    }
                            , state_t = 0
                            , state_globe = (globe_vs, globe_fs)
                            , state_globe_map = globe_map 
                            , state_position = 0
                            , state_tm = tm
                            }
  GLUT.displayCallback $= display state
  GLUT.idleCallback $= Just (idle state)
  GLUT.keyboardMouseCallback $= Just (keyboardMouse state)
  initfn
  GLUT.mainLoop
