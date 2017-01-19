module Image (display, dimension)
where

import ComplexNumber
import Data.Fixed

--controls

--size of the picture in pixels (only square pictures in this version)
dimension = 301 :: Int

--base of logarithm for getting the lightness of a pixel (in case you use Iterative shading); the greater the base,
--the darker the image - in between 2 and 3 is usually perfect
base = 2.7

--maximum number of iterations for the newtonian function
iterations = 100 :: Int

--parameter for generalized Newton iterations   z -> z - param*f'(z)/f(z)
param = ComplexNumber 1 0.925

--you may want to rescale the visible range (this means both real & imaginary axes are scaled <-range; range>)
range = 6.0

--shading
--Iterative - lightness depends on number of iterations required for the point on complex plane to converge to a root (Newton's iterations)
--Exponential - lightness depends only on 'light' parameter and modulus of a complex number - lightness = 1 - light^(-(modulus z))
--None - lightness is set to constant 0.5
shading = Exponential

--if shading is Exponential (or Iterative with number of iterations 0), greater light provides whiteshift, lower gives a darker image
light = 1.15 --don't go below 1, hello darkness my old friend

--and of course the function you're drawing
function z = z^6 - t*z^3 +q*z
	where
		t = ComplexNumber 16 4
		q = ComplexNumber 12 3


{-
Good luck with adjusting the parameters for some epic fractals!
The further param lies from 1+0i, the more iterations are needed to get anything interesting, most often there is no convergence at all.
When using Iterative shading, number of iterations must be adjusted for every function to get a pretty picture,
too few of them result in picture being almost 100% dark, too many of them make the picture less varied.
And remember, art requires patience!
-}

--end of controls
type Coordinates = (Int, Int)
data Shading = Iterative | Exponential | None
data Pixel = Pixel RGB
data RGB   = RGB Float Float Float
data HL    = HL (Maybe Float) Float
instance Show Pixel where
	show (Pixel (RGB r g b)) = show (round (r*255.0)) ++ " " ++ show (round (g*255.0)) ++ " " ++ show (round (b*255.0)) ++ "\n"


newtonian :: Int -> (ComplexNumber ->ComplexNumber) -> ComplexNumber -> (ComplexNumber, Int)
newtonian i f z
 	| i==iterations || t < h = (z, i)
	| otherwise = newtonian (i+1) f (z - t)
	where
		t = param * (f z)//(derive f z)
		h = ComplexNumber 0.00001 0.00001

--simplified HSL to RGB conversion, taking S = 1
hlToRGB :: HL -> RGB
hlToRGB (HL Nothing l) = RGB 0.0 0.0 0.0
hlToRGB (HL (Just h) l)
	| h >= 360.0   = hlToRGB (HL (Just (h-360.0)) l)
	| h < 0.0      = hlToRGB (HL (Just (h+360.0)) l)
	| h < 60.0     = RGB (c+m) (x+m) (m)
	| h < 120.0    = RGB (x+m) (c+m) (m)
	| h < 180.0    = RGB (m) (c+m) (x+m)
	| h < 240.0    = RGB (m) (x+m) (c+m)
	| h < 300.0    = RGB (x+m) (m) (c+m)
	| h < 360.0    = RGB (c+m) (m) (x+m)
	where
		c = (1.0 - abs (2*l - 1.0))
		m = l - c*0.5
		x = c * (1.0 - abs ( (mod' t 2.0) - 1.0)) where t = (h/60.0)
hlToRGB (HL (Just a) b) = RGB 1.0 1.0 1.0


getHL :: ComplexNumber -> HL
getHL z = HL (arg z) (getLightness shading z)


--lightness ranges from 0.0 to 1.0
getLightness :: Shading -> ComplexNumber -> Float
getLightness None z = 0.5
getLightness Iterative z | iterations /= 0 = log (2.0  - ((fromIntegral (snd (ev shading z))) / fromIntegral iterations)) / log base
getLightness _ z = 1 - (light**((-1)*(modulus z)))


transformFromPixel :: Coordinates -> ComplexNumber
transformFromPixel (x, y) = ComplexNumber (2* range * (fromIntegral x)/(fromIntegral (dimension-1)) - range) ( (-2.0)*range*(fromIntegral y)/(fromIntegral (dimension-1)) + range)


ev :: Shading -> ComplexNumber -> (ComplexNumber, Int)
ev Iterative z = newtonian 0 function z
ev _ z         = (function z, 0)


f :: ComplexNumber -> ComplexNumber
f z = fst (ev shading z)


pixels = [ [ show (Pixel (hlToRGB ( getHL ( f (transformFromPixel (x, y) ) ) ) ) ) | x<-[0..(dimension-1)]] | y<-[0..(dimension-1)] ]


display = foldr (++) "" (foldr (++) [] pixels)
