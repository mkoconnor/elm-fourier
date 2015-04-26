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

port inputScaling : Signal Float
                     
type alias Model = { elapsedTime : Time.Time, path : Path.Path, arcLength : Float, circleRadiusLength : Float, circleRadiusLength2 : Float, speedMultiplier : Float }

initialModel : Model
initialModel = { elapsedTime = 0, path = Path.empty { timeToKeepPoints = 4 * Time.second }, arcLength = 0, circleRadiusLength = 0, circleRadiusLength2 = 0, speedMultiplier = 2 }

currentPoint : Model -> (Float, Float)
currentPoint model =
 (model.circleRadiusLength * cos model.arcLength, model.circleRadiusLength * sin model.arcLength)

currentPoint2 : Model -> (Float, Float)
currentPoint2 model =
 let (x, y) = currentPoint model in
 let thisArcLength = model.speedMultiplier * model.arcLength in
 (x + model.circleRadiusLength2 * cos thisArcLength, y + model.circleRadiusLength2 * sin thisArcLength)

updateModel : Model -> { scaling : Float, timeSpan : Time.Time } -> Model
updateModel model { scaling, timeSpan } = 
  let newCircleRadiusLength = scaling in
  let newElapsedTime = model.elapsedTime + timeSpan in
  let newArcLength = 2 * pi * Time.inSeconds newElapsedTime * rotationsPerSecond in
  let newCircleRadiusLength2 =
    if model.circleRadiusLength2 == 0
    then 0.3
    else model.circleRadiusLength2
  in
  let newModel = { model | circleRadiusLength <- newCircleRadiusLength, arcLength <- newArcLength, elapsedTime <- newElapsedTime, circleRadiusLength2 <- newCircleRadiusLength2 }
  in
  let newPath = Path.pruneOld (Path.addPoint model.path { coords = currentPoint2 newModel, timeAdded = model.elapsedTime }) in
  { newModel | path <- newPath }
  
toElement : Model -> { width : Int, height : Int } -> E.Element
toElement model { width, height } = 
  let mult = 0.9 * (toFloat (min width height) / 2) / (model.circleRadiusLength + model.circleRadiusLength2) in
  let circle = C.outlined (C.solid Color.black) (C.circle (mult * model.circleRadiusLength)) in
  let curPoint = currentPoint model in
  let mulCurPoint = (mult * fst curPoint, mult * snd curPoint) in
  let radius = C.traced (C.solid Color.black) (C.segment (0,0) mulCurPoint) in
  let circle2 = C.move mulCurPoint (C.outlined (C.solid Color.black) (C.circle (mult * model.circleRadiusLength2))) in
  let curPoint2 = currentPoint2 model in
  let mulCurPoint2 = (mult * fst curPoint2, mult * snd curPoint2) in
  let radius2 = C.traced (C.solid Color.black) (C.segment mulCurPoint mulCurPoint2) in
  let path = Path.toForm model.path { multiplier = mult } in
  C.collage width height [circle, radius, path, circle2, radius2]

models : Signal Model
models = Signal.foldp (\(timeSpan, scaling) model -> updateModel model { scaling = scaling, timeSpan = timeSpan}) initialModel (Signal.map2 (\x y -> (x,y)) (Time.fps 60) inputScaling)
  
main = Signal.map2 (\model {width, height} -> toElement model {width=width, height=height}) models scaledDimensions
