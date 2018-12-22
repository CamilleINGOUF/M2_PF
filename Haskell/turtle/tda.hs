import System.Random
import System.IO.Unsafe

data Color = Color RGB | Red | Blue | Green | Black | White | Yellow | Orange | Random deriving(Show)
data RGB = RGB Int Int Int deriving(Show)
data Shape = Circle Color (Int, Int) Int | Rect Color (Int, Int) Int Int | Line Color (Int, Int) [(Int,Int)] deriving(Show)
data Screen = Screen {
  width::Int,
  height::Int,
  shapes::[Shape]
} deriving(Show)
data Turtle = Turtle {
  x::Int,
  y::Int,
  oriantation::Float,
  draw::Bool
} deriving(Show)
data World = World {
  turtle::Turtle,
  screen::Screen
} deriving(Show)

getColor::Color -> String
getColor Red = (getRGB (RGB 255 0 0))
getColor Blue = (getRGB (RGB 0 0 255))
getColor Green = (getRGB (RGB 0 255 0))
getColor Black = (getRGB (RGB 0 0 0))
getColor White = (getRGB (RGB 255 255 255))
getColor Yellow = (getRGB (RGB 255 255 0))
getColor Orange = (getRGB (RGB 255 140 0))
getColor (Color rgb) = (getRGB rgb)
getColor Random = getRGB (RGB (myRandom (0,255)) (myRandom (0,255)) (myRandom (0,255)))

getRGB::RGB -> String
getRGB (RGB r g b) = "rgb("++(show r)++","++(show g)++","++(show b)++")"

convert::Shape -> String
convert (Circle color (x,y) r) = "<circle cx=\""++(show x)++"\" cy=\""++(show y)++"\" r=\""++(show r)++"\" fill=\""++(getColor color)++"\"/>"
convert (Rect color (x, y) w h) = "<rect x=\""++(show x)++"\" y=\""++(show y)++"\" width=\""++(show w)++"\" height=\""++(show h)++"\"  fill=\""++(getColor color)++"\"/>"
convert (Line color (x, y) l) = "<path d=\"M "++(show x)++" "++(show y)++" "++(getLines l)++"\" stroke=\""++(getColor color)++"\" stroke-width=\"3\" fill=\"none\"/>"

getLines::[(Int, Int)] -> String
getLines [] = ""
getLines ((x,y):ls) = "l "++(show x)++" "++(show y)++" "++(getLines ls)

makeHtml::Screen -> String
makeHtml (Screen w h shapes) = "<html><body><svg width=\""++(show w)++"\" height=\""++(show h)++"\">"++(foldl (++) "" (map convert shapes))++"</svg></body></html>"

export::Screen -> IO()
export screen = writeFile "turtle.html" (makeHtml screen)

myRandom::(Int,Int) -> Int
myRandom (m,n) = unsafePerformIO (getStdRandom (randomR (m, n)))

randomShapes::[Shape]
randomShapes = s
  where
    l = (Line Random ((myRandom (0,300)),(myRandom (0,300))) [(50,80), (-60,10), (20,-70)])
    c = (Circle Random ((myRandom (0,300)),(myRandom (0,300))) 50)
    r = (Rect Random ((myRandom (0,300)),(myRandom (0,300))) 60 50)
    s = [c,r,l]

initWorld::World
initWorld = World (Turtle 0 0 0 True) (Screen 500 500 [Line Random (250,250) []])

calculY::Int -> Float -> Integer
calculY dist angle = toInteger (round ((sin angle) * (fromInteger (toInteger dist))))

calculX::Int -> Float -> Integer
calculX dist angle = toInteger (round ((cos angle) * (fromInteger (toInteger dist))))

up::World -> World
up ((World (Turtle x y o _) (Screen w h shapes))) = World (Turtle x y o False) (Screen w h shapes)

down::World -> World
down ((World (Turtle x y o _) (Screen w h shapes))) = World (Turtle x y o True) (Screen w h (shapes++[Line Random (x,y) []]))

rotate::World -> Float -> World
rotate ((World (Turtle x y o draw) (Screen w h shapes))) r = World (Turtle x y (o+r) draw) (Screen w h shapes)

forward::World -> Int -> World
forward (World (Turtle x y o True) (Screen w h shapes)) d = World t s
    where 
      xx = fromIntegral (calculX d o)
      yy = fromIntegral (calculY d o)
      t = Turtle xx yy o True
      (Line col (lx,ly) l) = (last shapes)
      s = Screen w h ((init shapes)++[Line col (lx,ly) (l++[(xx,yy)])])
forward w _ = w

square::World -> Int -> World
square w c = (forward (rotate (forward (rotate (forward (rotate (forward w c) (pi/2)) c) (pi/2)) c) (pi/2)) c)

polygone::World -> Int -> Float -> Int -> World
polygone w _ _ 0 = w
polygone w c max n = rotate (forward (polygone w c max (n-1)) c) (pi/(max/2))

blade::World -> Int -> World
blade w c = forward (rotate (polygone (forward w c) c 4 4) (-pi)) c

mill::World -> Int -> World
mill w c = blade4
      where
        blade1 = blade (rotate w (pi/4))c
        blade2 = blade (rotate blade1 (pi/2)) c
        blade3 = blade (rotate blade2 (pi/2)) c
        blade4 = blade (rotate blade3 (pi/2)) c

main::IO()
main = do
  -- export (Screen 1000 1000 randomShapes)
  -- export (screen (square w 60))
  -- export (screen (polygone w 5 46 46))
  export (screen (mill init 60))
  where
    init = initWorld 
    w = rotate init (pi/4)