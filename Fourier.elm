module Fourier where
import Maybe
import Time
import Color
import Text
import Graphics.Collage as C
import Graphics.Element as E
import Signal
import Window
import Path

positionDistance : (Float, Float) -> Float
positionDistance (x,y) = sqrt (x * x + y * y)

scaledDimensions : Signal { width : Int, height : Int }
scaledDimensions = Signal.map (\(width, height) -> { width = width, height = height }) Window.dimensions
-- scaledDimensions = Signal.map (\(width, height) -> { width = round (toFloat width / 2), height = height } ) Window.dimensions

rotationsPerSecond = 1/4

port inputRadii : Signal (List Float)

port showCircles : Signal Bool

type alias Model = { elapsedTime : Time.Time, path : Path.Path, radii : List Float, centers : List (Float, Float) }

initialModel : Model
initialModel = { elapsedTime = 0, path = Path.empty { timeToKeepPoints = 4 * Time.second }, radii = [1, 1], centers = [(1,0), (2,0)] }

currentRevCenters : Model -> List (Float, Float)
currentRevCenters model =
  let arcLength = 2 * pi * Time.inSeconds model.elapsedTime * rotationsPerSecond in
  let (centers, _, _) = 
    List.foldl (\radius (acc, (last_x, last_y), arcLength) ->
        let thisArcLength = 2 * arcLength in
        let (this_x, this_y) = (last_x + radius * cos thisArcLength, last_y + radius * sin thisArcLength) in
    ((this_x, this_y)::acc, (this_x, this_y), thisArcLength)) ([], (0,0), arcLength / 2) model.radii
  in 
  centers

updateModel' : Model -> { radii : List Float, timeSpan : Time.Time, prune : Bool } -> Model
updateModel' model { radii, timeSpan, prune } = 
  let newElapsedTime = model.elapsedTime + timeSpan in
  let newModel = { model | elapsedTime <- newElapsedTime, radii <- radii } in
  let newRevCenters = currentRevCenters newModel in
  case List.head newRevCenters of
      Nothing -> newModel
      Just newPoint -> 
        let newPath =
           if radii == model.radii
           then
              let path = Path.addPoint model.path { coords = newPoint, timeAdded = newElapsedTime } in
              if prune then Path.pruneOld path
              else path
           else Path.empty { timeToKeepPoints = model.path.timeToKeepPoints }
       in
       -- just save time by not reversing if we're not at the last point in the loop
       { newModel | path <- newPath, centers <- if prune then List.reverse newRevCenters else newRevCenters
       }

upto from to = if to <= from then [] else from :: (upto (from + 1) to)

updateModel model { radii, timeSpan } =
  let numFakes = List.length radii * 4 in
  let fake = { radii = radii, timeSpan = timeSpan / toFloat numFakes } in
  List.foldl (\i model -> updateModel' model { fake | prune = i == numFakes - 1}) model (upto 0 numFakes)

maxDistOfRadii : List Float -> Float
maxDistOfRadii l = fst (List.foldl (\radius (prevMax,prevXCoord) -> 
   let thisXCoord = prevXCoord + radius in
   (max prevMax (thisXCoord + radius), thisXCoord)) (0,0) l)

toElement : Model -> { width : Int, height : Int, showCircles:Bool } -> E.Element
toElement model { width, height, showCircles } = 
  let mult = (toFloat (min width height) / 2) / maxDistOfRadii model.radii in
  let centers = List.map (\(x, y) -> (mult * x, mult * y)) model.centers in
  let circlesAndRadii =
    if not showCircles
    then []
    else 
      let circles = List.map2 (\center radius -> 
       C.move center (C.outlined (C.solid Color.black) (C.circle (mult * radius)))) ((0,0)::centers) model.radii in
       let radii = List.map2 (\center1 center2 ->
            C.traced (C.solid Color.black) (C.segment center1 center2)) ((0,0)::centers) centers
       in
       List.append circles radii
  in       
  let path = Path.toForm model.path { multiplier = mult } in
  C.collage width height (path :: circlesAndRadii)

models : Signal Model
models = Signal.foldp (\(timeSpan, radii) model -> updateModel model { radii = radii, timeSpan = timeSpan}) initialModel (Signal.map2 (\x y -> (x,y)) (Time.fps 60) inputRadii)
  
main = Signal.map3 (\model {width, height} showCircles -> toElement model {width=width, height=height, showCircles = showCircles}) models scaledDimensions showCircles
