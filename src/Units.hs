module Units where

--
-- Units of measure
--

data Meter
data Kilogram
data Second

--
-- A base quentity is just a wrapper around Double
-- along with a phantom type.
-- eg. type Length = BaseQuantity Meter
--

newtype BaseQuantity a = BaseQuantity Double

--
-- A quotient of two units a and b is a/b.
-- we will represent it as a function b -> a
-- eg. Meter/Second => Second -> Meter
--

type Quotient a b = b -> a

--
-- There are dimensionless values (eg Pi)
--

type Dimensionless = BaseQuantity ()

--
-- The inverse 1/a
--

type Inverse a = Quotient Dimensionless a

--
-- A product of two units a *b can be writter as a/b^-1
--

type Product a b = Quotient a (Inverse b)

--
-- A square of a is a * a
-- 

type Square a = Product a a


--
-- All quantities are constructed from a double value.
--

class Quantity a where
    construct :: Double -> a
    destruct :: a -> Double

instance Quantity (BaseQuantity a) where
    construct = BaseQuantity
    destruct (BaseQuantity a) = a

instance (Quantity a, Quantity b) => Quantity (a -> b) where
    construct a = \b -> construct (a * destruct b)
    destruct x = destruct ( x (construct 1))

infixl 6 .+.
(.+.) :: Quantity a => a -> a -> a
(.+.) x y = construct $ (destruct x) + (destruct y)

-- We can subtract two quantities of the same unit.
infixl 6 .-.
(.-.) :: Quantity a => a -> a -> a
(.-.) x y = construct $ (destruct x) - (destruct y)

-- We can multiply any two quantities.
infixl 7 .*.
(.*.) :: (Quantity a, Quantity b) => a -> b -> Product a b
(.*.) x y z = construct $ destruct (z y) * destruct x

-- We can divide any two quantities.
infixl 7 ./.
(./.) :: (Quantity a, Quantity b) => a -> b -> Quotient a b
(./.) x y z = construct $ destruct z * destruct x / destruct y

type Length = BaseQuantity Meter
type Mass = BaseQuantity Kilogram
type Time = BaseQuantity Second
type Area = Square Length
type Velocity = Quotient Length Time 
