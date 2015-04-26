module Fourier where
import Maybe
import Time
import Color
import Text
import Graphics.Collage as C
import Graphics.Element as E
import Signal
import Window
import Mouse
import Path

positionDistance : (Float, Float) -> Float
positionDistance (x,y) = sqrt (x * x + y * y)

scaledDimensions : Signal { width : Int, height : Int }
scaledDimensions = Signal.map (\(width, height) -> { width = width, height = height }) Window.dimensions
-- scaledDimensions = Signal.map (\(width, height) -> { width = round (toFloat width / 2), height = height } ) Window.dimensions

realMousePosition : Signal (Float, Float)
realMousePosition = Signal.map2 (\{width,height} (x,y) -> (toFloat x - toFloat width/2,toFloat height/2 - toFloat y)) scaledDimensions Mouse.position

mouseScaling : Signal Float
mouseScaling =
  let s =  Signal.foldp (\(isDown,position) (firstPositionWhenDown,overallScalingPlusSinceDown,overallScaling) ->
   let newFirstPos =
     if not isDown
     then Nothing
     else Just (Maybe.withDefault position firstPositionWhenDown)
   in
   let newOverallScaling =
     if not isDown
     then overallScalingPlusSinceDown
     else overallScaling
   in
   let newOverallScalingPlusSinceDown =
     if not isDown
     then overallScalingPlusSinceDown
     else
       case firstPositionWhenDown of
          Nothing -> overallScalingPlusSinceDown
          Just pos -> (positionDistance position / positionDistance pos) * overallScaling
   in
   (newFirstPos,newOverallScalingPlusSinceDown,newOverallScaling)) (Nothing,1,1) (Signal.map2 (\x y -> (x,y)) Mouse.isDown realMousePosition)
   in
   Signal.map (\(_,y,_) -> y) s

rotationsPerSecond = 1/4

port inputRadii : Signal (List Float)
                     
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

updateModel : Model -> { radii : List Float, timeSpan : Time.Time } -> Model
updateModel model { radii, timeSpan } = 
  let newElapsedTime = model.elapsedTime + timeSpan in
  let newModel = { model | elapsedTime <- newElapsedTime, radii <- radii } in
  let newRevCenters = currentRevCenters newModel in
  case List.head newRevCenters of
      Nothing -> newModel
      Just newPoint -> 
        let newPath =
           if radii == model.radii
           then 
              Path.pruneOld (Path.addPoint model.path { coords = newPoint, timeAdded = newElapsedTime })
           else Path.empty { timeToKeepPoints = model.path.timeToKeepPoints }
       in   
       { newModel | path <- newPath, centers <- List.reverse newRevCenters }

maxDistOfRadii : List Float -> Float
maxDistOfRadii l = fst (List.foldl (\radius (prevMax,prevXCoord) -> 
   let thisXCoord = prevXCoord + radius in
   (max prevMax (thisXCoord + radius), thisXCoord)) (0,0) l)

toElement : Model -> { width : Int, height : Int } -> E.Element
toElement model { width, height } = 
  let mult = (toFloat (min width height) / 2) / maxDistOfRadii model.radii in
  let centers = List.map (\(x, y) -> (mult * x, mult * y)) model.centers in
  let circles = List.map2 (\center radius -> 
     C.move center (C.outlined (C.solid Color.black) (C.circle (mult * radius)))) centers model.radii in
  let radii = List.map2 (\center1 center2 ->
    C.traced (C.solid Color.black) (C.segment center1 center2)) ((0,0)::centers) centers
  in
  let path = Path.toForm model.path { multiplier = mult } in
  C.collage width height (path :: List.append circles radii)

models : Signal Model
models = Signal.foldp (\(timeSpan, radii) model -> updateModel model { radii = radii, timeSpan = timeSpan}) initialModel (Signal.map2 (\x y -> (x,y)) (Time.fps 60) inputRadii)
  
main = Signal.map2 (\model {width, height} -> toElement model {width=width, height=height}) models scaledDimensions
