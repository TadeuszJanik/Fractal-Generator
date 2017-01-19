module ComplexNumber
where

data ComplexNumber = ComplexNumber Float Float

instance Num ComplexNumber where
	ComplexNumber a b + ComplexNumber a' b' = ComplexNumber (a+a') (b+b')
	ComplexNumber a b - ComplexNumber a' b' = ComplexNumber (a-a') (b-b')
	ComplexNumber a b * ComplexNumber a' b' = ComplexNumber (a*a' - b*b') (a*b' + a'*b)
instance Show ComplexNumber where
	show (ComplexNumber a b) = show a ++ (if b >= 0 then "+" else "") ++ show b ++ "i"
instance Eq ComplexNumber where
	ComplexNumber a b == ComplexNumber a' b' = a == a' && b == b'

-- this is only for convenience later on, such a formal definition doesn't exist
instance Ord ComplexNumber where
	z1 > z2 = modulus z1 > modulus z2
	z1 < z2 = modulus z1 < modulus z2

derive :: (ComplexNumber -> ComplexNumber) -> ComplexNumber -> ComplexNumber
derive f z = (f (z+h) - f z) // h
	where h = ComplexNumber 0.0001 0.0001 -- approximation of course

modulus :: ComplexNumber -> Float
modulus (ComplexNumber a b) = sqrt (a^2 + b^2)

arg :: ComplexNumber -> Maybe Float
arg (ComplexNumber a b)
  | (a^2 + b^2) == 0  = Nothing
  | b >= 0 = Just ( 360.0*(acos (a/(sqrt (a^2 + b^2))))/2/pi)
	| otherwise = Just ( 360.0 - 360.0*(acos (a/(sqrt (a^2 + b^2))))/2/pi)

argToRadians :: Maybe Float -> Float
argToRadians (Just a) = 2*pi*a/360.0


(//) :: ComplexNumber -> ComplexNumber -> ComplexNumber
ComplexNumber a b // ComplexNumber 0 0 = ComplexNumber 1000000.0 1000000.0 --this simulates infinity
ComplexNumber a b // ComplexNumber c d = ComplexNumber ((a*c+b*d)/(c^2 + d^2)) ((b*c-a*d)/(c^2 + d^2))


expForm :: ComplexNumber -> (Float, Maybe Float)
expForm z = (modulus z, arg z)


complexExp :: ComplexNumber -> ComplexNumber
complexExp = algebraicForm . complexExp'


complexExp' :: ComplexNumber -> (Float, Maybe Float)
complexExp' (ComplexNumber a b) = ((exp 1)**a, Just b)


algebraicForm :: (Float, Maybe Float) -> ComplexNumber
algebraicForm (_, Nothing) = ComplexNumber 0 0
algebraicForm (m, a) = ComplexNumber (m * cos (argToRadians a)) (m*sin(argToRadians a))
