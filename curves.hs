data Point = Point Double Double deriving (Show)
data Curve = Curve Point [Point] deriving (Show)

--------- Point ------------

point :: (Double, Double) -> Point
point (x,y) = (Point x y)

instance Eq Point where
  p1 == p2 = abs (pointX p1 - pointX p2) < 0.01 && abs (pointY p1 - pointY p2) < 0.01

pointX :: Point -> Double
pointX (Point x _) = x

pointY :: Point -> Double
pointY (Point _ y) = y

--takes radians
rotatePoint:: Double -> Point -> Point
rotatePoint angle (Point x y) = (point ((x * cos(angle) - y * sin(angle)),(x * sin(angle) + y * cos(angle))))  

-- vx & vy are vector coordinates
translatePoint :: Point -> Point -> Point
translatePoint (Point vx vy) (Point x y) = (point ((x + vx),(y + vy)))


--------- Curve --------------

curve :: Point -> [Point] -> Curve
curve p pl = (Curve p pl)

connect :: Curve -> Curve -> Curve
------connect (Curve s1 []) (Curve s2 l2) = (curve s1 (s2:l2))
connect (Curve s1 l1) (Curve s2 l2) = (curve s1 (l1++s2:l2))

rotate :: Curve -> Double -> Curve
rotate (Curve s l) degAngle =  (curve (rotatePoint (toRadians degAngle) s ) (map (rotatePoint (toRadians degAngle)) l ) )

translate :: Curve -> Point -> Curve
translate (Curve s l) p
  | s /= p = (curve (translatePoint s vector) (map (translatePoint vector) l))
  | otherwise = (curve s l) 
  where vector = point (pointX p - pointX s, pointY p - pointY s)


-------- Utils ------------------

toRadians:: Double -> Double
toRadians x = x * pi/180

toDegrees:: Double -> Double
toDegrees x = x * 180/pi



main :: IO ()

main = do let p1 = point (0.001, 1.001)
              p0 = point (1.0, 1.0)
              p2 = point (0.500, 1.50)
              p3 = point (2.000, 2.00)
              p4 = point (1.000, 0.00)
              p5 = point (1.0, 2.0)
              x1 = pointX p1
              y1 = pointY p1
              c0 = curve p0 []
              c1 = curve p1 [p2,p3]
              c2 = curve p1 [p4,p3]
              result = p1 == p2
              connected = connect c0 c1
              deg1 = toRadians 30.0
              rad = toDegrees 0.5235987755982988
              rotatedPoint = rotatePoint (pi/2) p4
              rotatedCurve = rotate c2 90
              translatedCurve = translate c1 p5
          putStrLn $ "Initial point: " ++ show p1
          putStrLn $ "x: " ++ show x1
          putStrLn $ "y: " ++ show y1
          putStrLn $ "EQ: " ++ show result
          putStrLn $ "Curve: " ++ show c1 ++ "  |  c0: " ++ show c0
          putStrLn $ "connected: " ++ show connected
          putStrLn $ "rad: " ++ show deg1
          putStrLn $ "deg: " ++ show rad
          putStrLn $ "rotatedPoint: " ++ show rotatedPoint
          putStrLn $ "rotatedCurve: " ++ show rotatedCurve
          putStrLn $ "translatedCurve:" ++ show translatedCurve ++ "\ninitial curve: " ++ show c1 ++ "\ninitial point: " ++ show p1 ++"\ntranslate point: " ++ show p5

          


