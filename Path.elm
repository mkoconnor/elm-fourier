module Path (Path, PointOnPath, empty, addPoint, toForm, pruneOld) where
import Time
import List
import Graphics.Collage as C
import Color

type alias PointOnPath = { coords : (Float, Float), timeAdded : Time.Time }

type alias Path = { points : List PointOnPath, timeToKeepPoints : Time.Time, closed : Bool }

empty : { timeToKeepPoints : Time.Time } -> Path
empty { timeToKeepPoints } = { points = [], timeToKeepPoints = timeToKeepPoints , closed = False}

addPoint : Path -> PointOnPath -> Path
addPoint path point =
    if path.closed
    then path
    else        
        { points = point :: path.points, timeToKeepPoints = path.timeToKeepPoints, closed = False }

pruneOld : Path -> Path
pruneOld path =
  if path.closed
  then path
  else 
    case path.points of
       [] -> path
       lastPoint :: _ -> 
          let cutoff = lastPoint.timeAdded - path.timeToKeepPoints in
          if List.any (\point -> point.timeAdded < cutoff) path.points
          then { path | closed <- True }
          else path

defaultLine = C.defaultLine
lineStyle = { defaultLine | color <- Color.blue, width <- 3, join <- C.Smooth, cap <- C.Round }

toForm : Path -> { multiplier : Float } -> C.Form
toForm path { multiplier } =
   C.traced lineStyle (C.path (List.map (\point -> (multiplier * fst point.coords, multiplier * snd point.coords)) path.points))

