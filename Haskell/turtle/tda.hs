import System.Random

data Color = Color RGB | Red | Blue | Green | Black | White | Yellow | Orange deriving(Show)
data RGB = RGB Int Int Int deriving(Show)
data Shape = Circle Color (Int, Int) Int | Rect Color (Int, Int) Int Int | Line Color (Int, Int) [(Int,Int)] deriving(Show)
data Screen = Screen {
  width::Int,
  height::Int,
  shapes::[Shape]
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

main::IO()
main = do
  export (Screen 1000 1000 [c,r,l])
  where c = (Circle (Color (RGB 234 78 98)) (50,50) 50)
        r = (Rect Blue (100,100) 60 50)
        l = (Line Yellow (200,300) [(50,80), (-60,10), (20,-70)])