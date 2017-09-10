-- module Curves
-- ( Point,
--   Curve,
--   Line(..),
--   point,
--   pointX,
--   pointY,
--   rotatePoint,
--   translatePoint,
--   curve,
--   connect,
--   rotate,
--   normalize,
--   translate,
--   reflect,
--   bbox,
--   width,
--   height,
--   toList,
--   toSVG,
--   toFile,
-- ) where

import Text.Printf

data Point = Point Double Double deriving (Show)
data Curve = Curve Point [Point] deriving (Show)
data Line = Vertical Integer | Horizontal Integer deriving (Show)

-- ------- Point ------------

point :: (Double, Double) -> Point
point (x,y) = Point x y

instance Eq Point where
  p1 == p2 = abs (pointX p1 - pointX p2) < 0.01 && abs (pointY p1 - pointY p2) < 0.01

pointX :: Point -> Double
pointX (Point x _) = x

pointY :: Point -> Double
pointY (Point _ y) = y

-- takes radians
rotatePoint:: Double -> Point -> Point
rotatePoint angle (Point x y) = point (x * cos angle - y * sin angle, x * sin angle + y * cos angle)

-- vx & vy are vector coordinates
translatePoint :: Point -> Point -> Point
translatePoint (Point vx vy) (Point x y) = point (x + vx, y + vy)


-- ------- Curve --------------

curve :: Point -> [Point] -> Curve
curve = Curve

instance Eq Curve where
  (Curve s1 l1) == (Curve s2 l2) = s1 == s2 && l1 == l2

connect :: Curve -> Curve -> Curve
connect (Curve s1 l1) (Curve s2 l2) = curve s1 (l1++s2:l2)

rotate :: Curve -> Double -> Curve
rotate (Curve s l) degAngle =  curve (rotatePoint (toRadians degAngle) s ) (map (rotatePoint (toRadians degAngle)) l)


translate :: Curve -> Point -> Curve
translate (Curve s l) p
  | s /= p = curve (translatePoint s vector) (map (translatePoint vector) l)
  | otherwise = curve s l
  where vector = point (pointX p - pointX s, pointY p - pointY s)

reflect :: Curve -> Line -> Curve
reflect (Curve s l)(Vertical x) = curve (translatePoint s (multiply 2.0 (getVector s (point (fromIntegral x, pointY s))))) (recursiveReflect l (Vertical x))
reflect (Curve s l)(Horizontal y) = curve (translatePoint s (multiply 2.0 (getVector s (point (pointX s, fromIntegral y))))) (recursiveReflect l (Horizontal y))

recursiveReflect :: [Point] -> Line -> [Point]
recursiveReflect [] _ = []
recursiveReflect [h] (Vertical x) = [translatePoint h (multiply 2.0 (getVector h (point (fromIntegral x, pointY h))))]
recursiveReflect (h:t) (Vertical x) = translatePoint h (multiply 2.0 (getVector h (point (fromIntegral x, pointY h)))):recursiveReflect t (Vertical x)
recursiveReflect [h] (Horizontal y) = [translatePoint h (multiply 2.0 (getVector h (point (pointX h, fromIntegral y))))]
recursiveReflect (h:t) (Horizontal y) =  translatePoint h (multiply 2.0 (getVector h (point (pointX h, fromIntegral y)))):recursiveReflect t (Horizontal y)

bbox :: Curve -> (Point, Point)
bbox (Curve s l) = (point (minimum xs, minimum ys), point (maximum xs, maximum ys))
  where ys = [pointY p | p <- s:l]
        xs = [pointX p | p <- s:l]

width :: Curve -> Double
width (Curve s l) = pointX p2 - pointX p1
 where (p1,p2) = bbox(Curve s l)

height :: Curve -> Double
height (Curve s l) = pointY p2 - pointY p1
 where  (p1,p2) = bbox(Curve s l)

toList :: Curve -> [Point]
toList (Curve s l) = s:l

normalize :: Curve -> Curve
normalize (Curve s l) = translate (Curve s l) secretPoint
   where (p1,_) = bbox(Curve s l)
         secretPoint  = point (- pointX p1 + pointX s, - pointY p1 + pointY s)

toSVG :: Curve -> String
toSVG (Curve s []) = "<svg xmlns=\"http://www.w3.org/2000/svg\"\n" ++
                        printf "width=\"2.00fpx\" height=\"2.00px\" version=\"1.1\"><g>" ++
                          "\n<line style=\"stroke-width: 2px; stroke: black; fill:white\"\n" ++
                            printf "x1=\"%.2f\" x2=\"%.2f\" y1=\"%.2f\" y2=\"%.2f\" />\n" (pointX s) (pointY s) (pointX s) (pointY s) ++
                              "</g>\n</svg>"
toSVG (Curve s [t]) = "<svg xmlns=\"http://www.w3.org/2000/svg\"\n" ++
                        printf "width=\"%.2fpx\" height=\"%.2fpx\" version=\"1.1\"><g>" boxHeight boxWidth ++
                          "\n<line style=\"stroke-width: 2px; stroke: black; fill:white\"\n" ++
                            printf "x1=\"%.2f\" x2=\"%.2f\" y1=\"%.2f\" y2=\"%.2f\" />\n" (pointX s) (pointY s) (pointX t) (pointY t) ++
                              "</g>\n</svg>"
  where boxHeight = height (curve s [t])
        boxWidth = width (curve s [t])

toSVG (Curve s l) = "<svg xmlns=\"http://www.w3.org/2000/svg\"\n" ++
                          printf "width=\"%.2fpx\" height=\"%.2fpx\" version=\"1.1\"><g>" boxHeight boxWidth ++
                            toSVGRecursive (curve s l)
  where boxHeight = height (curve s l)
        boxWidth = width (curve s l)


toSVGRecursive :: Curve -> String
toSVGRecursive (Curve _ []) = ""
toSVGRecursive (Curve h [t]) = "\n<line style=\"stroke-width: 2px; stroke: black; fill:white\"\n" ++
                                  printf "x1=\"%.2f\" x2=\"%.2f\" y1=\"%.2f\" y2=\"%.2f\" />\n" (pointX h) (pointY h) (pointX t) (pointY t) ++
                                    "</g>\n</svg>"
toSVGRecursive (Curve s (h:t)) = "\n<line style=\"stroke-width: 2px; stroke: black; fill:white\"\n" ++
                                  printf "x1=\"%.2f\" x2=\"%.2f\" y1=\"%.2f\" y2=\"%.2f\" />" (pointX s) (pointY s) (pointX h) (pointY h) ++
                                    toSVGRecursive (curve h t)

toFile :: Curve -> FilePath -> IO ()
toFile c filePath = writeFile filePath (toSVG $ normalize $ c)


-- ------ Utils ------------------

toRadians :: Double -> Double
toRadians x = x * pi/180

getVector :: Point -> Point -> Point
getVector (Point x1 y1) (Point x2 y2) = point (x2-x1,y2-y1)

multiply:: Double -> Point -> Point
multiply p (Point x y)  = point (x*p,y*p)

hilbert :: Curve -> Curve
hilbert c = c0 `connect` c1 `connect` c2 `connect` c3
   where  w = width c
          h = height c
          p = 6

          ch = reflect c $ Vertical 0

          c0 = ch `rotate` (-90) `translate` point (w+p+w, h+p+h)
          c1 = c `translate` point (w+p+w, h)
          c2 = c
          c3 = ch `rotate` 90 `translate` point (0, h+p)

main :: IO ()

main = do let p1 = point (1.0e-3, 1.001)
              p0 = point (1.0, 1.0)
              p2 = point (0.5, 1.5)
              p3 = point (2.0, 2.0)
              p4 = point (1.0, 0.0)
              p5 = point (1.0, 2.0)
              p6 = point (-90.0, -91.0)
              p7 = point (-2.0, 90.0)
              x1 = pointX p1
              y1 = pointY p1
              c0 = curve p0 []
              c1 = curve p1 [p2, p3]
              c2 = curve p1 [p4, p3]
              c3 = curve p1 [p5]
              c4 = curve p1 [p6, p7]
              result = p1 == p2
              connected = connect c0 c1
              deg1 = toRadians 30.0
              rotatedPoint = rotatePoint (pi / 2) p4
              rotatedCurve = rotate c2 90
              translatedCurve = translate c1 p5
              line1 = Horizontal 2
              reflectedCurve = reflect c3 line1
              box = bbox c4
              widthVal = width c4
              heightVal = (height c4)
              listRep = toList c4
              normalizedCurve = normalize c4
              svg = toSVG $ normalize c4
              c5 = hilbert $ hilbert $ hilbert $ hilbert $ curve (point (0,0)) []
          toFile c5 "file.svg"    
          putStrLn $ "Initial point: " ++ show p1
          putStrLn $ "x: " ++ show x1
          putStrLn $ "y: " ++ show y1
          putStrLn $ "EQ: " ++ show result
          putStrLn $ "Curve: " ++ show c1 ++ "  |  c0: " ++ show c0
          putStrLn $ "connected: " ++ show connected
          putStrLn $ "rad: " ++ show deg1
          putStrLn $ "rotatedPoint: " ++ show rotatedPoint
          putStrLn $ "rotatedCurve: " ++ show rotatedCurve
          putStrLn $
            "translatedCurve:" ++
              show translatedCurve ++
                "\ninitial curve: " ++
                  show c1 ++
                    "\ninitial point: " ++ show p1 ++ "\ntranslate point: " ++ show p5
          putStrLn $
            "----------------------\ninitial curve: " ++
              show c3 ++
                "\nreflection line" ++
                  show line1 ++
                    "\nreflectedCurve: " ++
                      show reflectedCurve ++ "\n---------------------"
          putStrLn $
            "----------------------\ninitial curve: " ++
              show c4 ++ "\nbbox" ++ show box ++ "\n---------------------"
          putStrLn $
            "width: " ++ show widthVal ++ " height: " ++ show heightVal
          putStrLn $ "as ilst: " ++ show listRep
          putStrLn $ "normalizedCurve: " ++ show normalizedCurve
          putStrLn $ "svg: " ++ show svg
