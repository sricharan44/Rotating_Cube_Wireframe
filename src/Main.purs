module Main where
import Prelude
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Control.Monad.Eff.DOM (addEventListener, querySelector)
import Control.Monad.Eff.Ref (REF, newRef, readRef, writeRef, modifyRef, Ref)
import DOM (DOM)
import DOM.HTML (window)
import DOM.HTML.Types (Window)
import DOM.HTML.Window (requestAnimationFrame)
import Data.Foldable (for_)
import Data.Maybe (Maybe(..))
import Graphics.Canvas (CANVAS, Context2D, getCanvasElementById, getContext2D, setFillStyle, fillRect, moveTo, lineTo, withContext, setStrokeStyle, beginPath, closePath, stroke)
import Math (pi, cos, sin)
import Partial.Unsafe (unsafePartial)

-- 3D & 2D Point NEW objects --
newtype Point3D = Point3D {x :: Number,y :: Number ,z :: Number}
newtype Point2D = Point2D {x :: Number,y :: Number}
newtype Angle3D = Angle3D {qx :: Number,qy :: Number,qz :: Number}

-- Cube object --
newtype Cube = Cube {x :: Number,y :: Number,z :: Number,size :: Number,color :: String}

-- Function to project 3D point on 2D coordinate plane
project :: Point3D -> Angle3D -> Point2D
project (Point3D { x, y, z }) (Angle3D { qx, qy, qz }) =
  let y_rotate_angle = (y * (cos qz) - x * (sin qz)) * cos(qx) + z *(sin qx)
      z_rotate_angle = z * (cos qx) - y_rotate_angle * (sin qx)
      x_rotate_angle = (x * (cos qz) + y * (sin qz)) * (cos qy) + z_rotate_angle * (sin qy)
  in
    Point2D { x: 250.0 + x_rotate_angle, y: 250.0 + y_rotate_angle } -- Where the cube has to rotate

withStroke :: forall e.
  Context2D -> String -> (Context2D -> Eff (canvas :: CANVAS | e) Context2D) -> Eff (canvas :: CANVAS | e) Context2D
withStroke ctx color draw = withContext ctx do
  ctx <- setStrokeStyle color ctx
  ctx <- beginPath ctx
  ctx <- draw ctx
  ctx <- closePath ctx
  stroke ctx

-- Function to draw line between two 2D points --
addEdge :: forall e. Context2D -> Point2D -> Point2D -> Eff (canvas :: CANVAS | e) Context2D
addEdge ctx (Point2D from) (Point2D to) = do
  ctx <- moveTo ctx from.x from.y
  lineTo ctx to.x to.y

-- v1,v2,v3,v4,v5,v6,v7,v8 - vertices of the CUBE --
drawCube :: forall e. Context2D -> Cube -> Angle3D -> Eff (canvas :: CANVAS | e) Context2D
drawCube ctx (Cube { color, x, y, z, size }) (Angle3D { qx, qy, qz })= do
  let v1 = project (Point3D { x: x - size, y: y - size, z: z - size }) (Angle3D {qx: qx, qy: qy, qz: qz})
  let v2 = project (Point3D { x: x - size, y: y + size, z: z - size }) (Angle3D {qx: qx, qy: qy, qz: qz})
  let v3 = project (Point3D { x: x - size, y: y - size, z: z + size }) (Angle3D {qx: qx, qy: qy, qz: qz})
  let v4 = project (Point3D { x: x - size, y: y + size, z: z + size }) (Angle3D {qx: qx, qy: qy, qz: qz})
  let v5 = project (Point3D { x: x + size, y: y - size, z: z - size }) (Angle3D {qx: qx, qy: qy, qz: qz})
  let v6 = project (Point3D { x: x + size, y: y + size, z: z - size }) (Angle3D {qx: qx, qy: qy, qz: qz})
  let v7 = project (Point3D { x: x + size, y: y - size, z: z + size }) (Angle3D {qx: qx, qy: qy, qz: qz})
  let v8 = project (Point3D { x: x + size, y: y + size, z: z + size }) (Angle3D {qx: qx, qy: qy, qz: qz})

  --Faces of the cube (Constructing and connecting)
  withStroke ctx color \ctx -> do
    ctx <- addEdge ctx v1 v5
    ctx <- addEdge ctx v5 v6
    ctx <- addEdge ctx v6 v2
    ctx <- addEdge ctx v2 v1
    ctx <- addEdge ctx v3 v7
    ctx <- addEdge ctx v7 v8
    ctx <- addEdge ctx v8 v4
    ctx <- addEdge ctx v4 v3
    ctx <- addEdge ctx v1 v3
    ctx <- addEdge ctx v5 v7
    ctx <- addEdge ctx v6 v8
    addEdge ctx v2 v4

-- function:animation of CUBE --
loopAnimation :: forall e state.
  Window -> Ref state -> state ->  (state -> Eff (ref :: REF, dom :: DOM | e) state) ->  Eff (ref :: REF, dom :: DOM | e) Unit
loopAnimation window ref state step = void $ requestAnimationFrame
    do loopAnimation window ref state step
       state <- readRef ref
       state <- step state
       writeRef ref state
    window

withAnimation :: forall e state.
  state ->  (state -> Eff (ref :: REF, dom :: DOM | e) state) -> Eff (ref :: REF, dom :: DOM | e) Unit
withAnimation state step = do
  window <- window
  ref <- newRef state
  loopAnimation window ref state step

withAnimateContext :: forall e state.
  String -> state -> (Context2D -> state -> Eff (dom :: DOM, ref :: REF, canvas :: CANVAS | e) state) -> Eff (dom :: DOM, ref :: REF, canvas :: CANVAS | e) Unit
withAnimateContext name state draw = do
  canvas <- getCanvasElementById name
  case canvas of
    Just canvas -> do
      ctx <- getContext2D canvas
      withAnimation state \state -> do
        draw ctx state
    Nothing -> pure unit

-- This function is used to fill the canvas background with color --
drawBackground :: forall e. Context2D -> Eff (canvas :: CANVAS | e) Context2D
drawBackground ctx = do
  ctx <- setFillStyle "rgb(0, 0, 0)" ctx
  fillRect ctx {x: 0.0,y: 30.0,w: 500.0,h: 500.0}			--Canvas Position and size--

-- state object --
state = {x: 0.0 ,y: 0.0 ,z: 0.0 ,qx: pi / 7.0 ,qy: pi / 7.0 ,qz: pi / 7.0 ,loop : 0.4}

-- This function is used to DECELERATE the CUBE when mouse is clicked(Deceleration happens in the same direction in all the axis at the same time) --
stopcube :: forall e.  Eff (dom :: DOM, ref :: REF, canvas :: CANVAS | e) Unit
stopcube =
  let
    -- getting the canvas element from the HTML --
    canvas = getCanvasElementById "mycanvas"
  in
    withAnimateContext "mycanvas" state \ctx state -> do
        ctx <- drawBackground ctx
        void $ drawCube ctx (Cube { x: state.x, y: state.y, z: state.z, size: 80.0, color: "rgb(255,255,255)" }) (Angle3D { qx: state.qx, qy: state.qy, qz: state.qz})
        pure $ state { x = state.x, y = state.y,z = state.z, qx = state.qx - state.loop, qy = state.qy- state.loop, qz = state.qz- state.loop, loop = max (state.loop - 0.01) 0.000}

--Function:construction and rotation of the CUBE in all axis--
startcube_xaxis :: forall e.  Eff (dom :: DOM, ref :: REF, canvas :: CANVAS | e) Unit
startcube_xaxis =
  let
    -- getting the canvas element from the HTML --
    canvas = getCanvasElementById "mycanvas"
  in
    withAnimateContext "mycanvas" state \ctx state -> do
        ctx <- drawBackground ctx
        void $ drawCube ctx (Cube { x: state.x, y: state.y, z: state.z, size: 80.0, color: "rgb(255,255,255)" }) (Angle3D { qx: state.qx, qy: state.qy, qz: state.qz})
        pure $ state { x = state.x, y = state.y,z = state.z,qx = state.qx + state.loop, qy = state.qy, qz = state.qz, loop = max (state.loop - 0.01) 0.000}

startcube_yaxis :: forall e.  Eff (dom :: DOM, ref :: REF, canvas :: CANVAS | e) Unit
startcube_yaxis =
  let
    -- getting the canvas element from the HTML --
    canvas = getCanvasElementById "mycanvas"
  in
    withAnimateContext "mycanvas" state \ctx state -> do
        ctx <- drawBackground ctx
        void $ drawCube ctx (Cube { x: state.x, y: state.y, z: state.z, size: 80.0, color: "rgb(255,255,255)" }) (Angle3D { qx: state.qx, qy: state.qy, qz: state.qz})
        pure $ state { x = state.x, y = state.y,z = state.z, qx = state.qx, qy = state.qy + state.loop, qz = state.qz, loop = max (state.loop - 0.01) 0.000}
startcube_zaxis :: forall e.  Eff (dom :: DOM, ref :: REF, canvas :: CANVAS | e) Unit
startcube_zaxis =
  let
    -- getting the canvas element from the HTML --
    canvas = getCanvasElementById "mycanvas"
  in
    withAnimateContext "mycanvas" state \ctx state -> do
        ctx <- drawBackground ctx
        void $ drawCube ctx (Cube {x: state.x, y: state.y, z: state.z, size: 80.0, color: "rgb(255,255,255)" }) (Angle3D { qx: state.qx, qy: state.qy, qz: state.qz})
        pure $ state {x = state.x, y = state.y,z = state.z,qx = state.qx, qy = state.qy, qz = state.qz + state.loop, loop = max (state.loop - 0.01) 0.000}

main :: Eff ( canvas :: CANVAS, ref :: REF, dom :: DOM, console :: CONSOLE ) Unit
main = void $ unsafePartial do
  flag <- newRef 0			--Flag:decide whether to call the startcube function or not when mouse is moved(Clicked/Swiped)--
  count <- newRef 0
  
  startcube_xaxis									-- Initially - Calling the startcube function along x axis--
  Just canvas <- getCanvasElementById "mycanvas"	-- getting the HTML canvas element by using its ID --
  ctx <- getContext2D canvas
  region <- querySelector "#mycanvas"

  for_ region $ addEventListener "click" $ void do
    modifyRef flag \a -> a + 1	-- Increasing the value of the flag reference Int --
    -- reading the value from the flag ref Int into a, so that we can compare it with int value(1) --
    a <- readRef flag
    if a == 1 					--On Click the CUBE stops as friction increases --
      then stopcube
      else log ""

  for_ region $ addEventListener "mousemove" $ void do
    --Loop decides which direction the cube has to rotate along when swiped--
    temp <- readRef flag
    modifyRef count \a -> mod (a + 1) 3
    c <- readRef count 
    if temp >= 1 && c == 0
      then do
        modifyRef flag \a -> 0
        startcube_xaxis			
      else log ""
    temp <- readRef flag
    if temp >= 1 && c == 2
      then do
        modifyRef flag \a -> 0
        startcube_yaxis
      else log ""
    temp <- readRef flag
    if temp >= 1 && c == 1
      then do
        modifyRef flag \a -> 0
        startcube_zaxis
      else log ""