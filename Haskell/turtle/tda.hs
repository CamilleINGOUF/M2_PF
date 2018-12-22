import System.Random
import System.IO.Unsafe

data Color = Color RGB | Red | Blue | Green | Black | White | Yellow | Orange | Random deriving(Show)
data RGB = RGB Int Int Int deriving(Show)
data Shape = Circle Color (Float, Float) Float | Rect Color (Float, Float) Float Float | Line Color (Float, Float) [(Float,Float)] deriving(Show)
data Screen = Screen {
  width::Int,
  height::Int,
  shapes::[Shape]
} deriving(Show)
data Turtle = Turtle {
  x::Float,
  y::Float,
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

getLines::[(Float, Float)] -> String
getLines [] = ""
getLines ((x,y):ls) = "l "++(show x)++" "++(show y)++" "++(getLines ls)

makeHtml::Screen -> String
makeHtml (Screen w h shapes) = "<html><body><svg width=\""++(show w)++"\" height=\""++(show h)++"\">"++(foldl (++) "" (map convert shapes))++"</svg></body></html>"

export::Screen -> String -> IO()
export screen name = writeFile name (makeHtml screen)

myRandom::(Int,Int) -> Int
myRandom (m,n) = unsafePerformIO (getStdRandom (randomR (m, n)))

randomShapes::[Shape]
randomShapes = s
  where
    l = (Line Random (fromIntegral (myRandom (0,300)),fromIntegral (myRandom (0,300))) [(50,80), (-60,10), (20,-70)])
    c = (Circle Random (fromIntegral (myRandom (0,300)),fromIntegral (myRandom (0,300))) 50)
    r = (Rect Random (fromIntegral (myRandom (0,300)),fromIntegral (myRandom (0,300))) 60 50)
    s = [c,r,l]

initWorld::World
initWorld = World (Turtle 0 0 0 True) (Screen 1500 1500 [Line Random (200,200) []])

calculY::Float -> Float -> Float
calculY dist angle = (sin angle) * dist

calculX::Float -> Float -> Float
calculX dist angle = (cos angle) * dist

up::World -> World
up ((World (Turtle x y o _) (Screen w h shapes))) = World (Turtle x y o False) (Screen w h shapes)

down::World -> World
down ((World (Turtle x y o _) (Screen w h shapes))) = World (Turtle x y o True) (Screen w h (shapes++[Line Random (x,y) []]))

rotate::World -> Float -> World
rotate ((World (Turtle x y o draw) (Screen w h shapes))) r = World (Turtle x y (o+r) draw) (Screen w h shapes)

forward::World -> Float -> World
forward (World (Turtle x y o True) (Screen w h shapes)) d = World t s
    where 
      xx = (calculX d o)
      yy = (calculY d o)
      t = Turtle xx yy o True
      (Line col (lx,ly) l) = (last shapes)
      s = Screen w h ((init shapes)++[Line col (lx,ly) (l++[(xx,yy)])])
forward w _ = w

square::World -> Float -> World
square w c = (forward (rotate (forward (rotate (forward (rotate (forward w c) (pi/2)) c) (pi/2)) c) (pi/2)) c)

polygone::World -> Float -> Float -> Int -> World
polygone w _ _ 0 = w
polygone w c max n = rotate (forward (polygone w c max (n-1)) c) (pi/(max/2))

-- moulin {
blade::World -> Float -> World
blade w c = forward (rotate (polygone (forward w c) c 4 4) (-pi)) c

mill::World -> Float -> World
mill w c = blade4
      where
        blade1 = blade (rotate w (pi/4))c
        blade2 = blade (rotate blade1 (pi/2)) c
        blade3 = blade (rotate blade2 (pi/2)) c
        blade4 = blade (rotate blade3 (pi/2)) c
-- } moulin

-- { Flocon
kochPiece::World -> Float -> Int -> World
kochPiece w c 0 = forward w c
kochPiece w c n = kochPiece (rotate (kochPiece (rotate (kochPiece (rotate (kochPiece w cc nn) o) cc nn) o2) cc nn) o) cc nn
    where o = (-pi/3)
          o2 = (2*pi/3)
          nn = n - 1
          cc = (c / 3)

koch::World -> Float -> Int -> Int-> World
koch w _ _ 0 = w
koch w c n m = koch (rotate (kochPiece w c n) (2*pi/3)) c n (m-1)
-- } flocon

main::IO()
main = do
  export (Screen 1000 1000 randomShapes) "aleatoire.html"
  export (screen (square w 60)) "carre.html"
  export (screen (polygone w 5 46 46)) "polygone.html"
  export (screen (mill w 60)) "moulin.html"
  export (screen (koch w 300 2 3)) "flocon.html"
  where 
    w = initWorld