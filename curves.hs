data Point = Point Double Double deriving (Show)
data Curve = Curve Point [Point] deriving (Show)


point :: (Double, Double) -> Point
point (x,y) = (Point x y)

pointX :: Point -> Double
pointX (Point x _) = x

pointY :: Point -> Double
pointY (Point _ y) = y

curve :: Point -> [Point] -> Curve
curve p pl = (Curve p pl)




instance Eq Point where
  p1 == p2 = abs (pointX p1 - pointX p2) < 0.01 && abs (pointY p1 - pointY p2) < 0.01

main :: IO ()

main = do let p1 = point (0.001, 1.001)
              p2 = point (0.500, 1.50)
              p3 = point (2.000, 2.00)
              x1 = pointX p1
              y1 = pointY p1
              c = curve p1 [p2,p3]
              result = p1 == p2
          putStrLn $ "Initial point: " ++ show p1
          putStrLn $ "x: " ++ show x1
          putStrLn $ "y: " ++ show y1
          putStrLn $ "EQ: " ++ show result
          putStrLn $ "Curve: " ++ show c


