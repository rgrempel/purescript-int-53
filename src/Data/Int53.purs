
-- | Purescript's built-in `Int` type is restricted to 32-bit integers. However, the
-- | Javascript runtime is capable of working with 53-bit integers. So, this module
-- | provides an `Int53` type, for cases where you want all 53 bits
-- | (from -9,007,199,254,740,991 to 9,007,199,254,740,991).
-- |
-- | ### Making an `Int53`
-- |
-- | To create an `Int53` from a `Number`, use [`fromNumber`](#v:fromNumber) or, more often,
-- | [`ceil`](#v:ceil), [`floor`](#v:floor), [`round`](#v:round) or [`truncate`](#v:truncate).
-- |
-- | To create an `Int53` from an `Int`, use [`fromInt`](#v:fromInt).
-- |
-- | If you're starting from a `String`, then there is [`fromString`](#:v:fromString).
-- |
-- | ### Using an `Int53`
-- |
-- | Once you have an `Int53`, you can do things like add, subtract, multiply,
-- | etc., in the usual way, since the ordinary classes for arithmetic are
-- | implemented. It should feel pretty much like using an ordinary integer.
-- |
-- | There are also functions for [`even`](#v:even), [`odd`](#v:odd), and [`pow`](#v:pow).
-- |
-- | ### Converting an `Int53` to something else
-- |
-- | When you need a `Number` again, you can use [`toNumber`](#v:toNumber) -- and when you need
-- | an `Int`, there is [`toInt`](#v:toInt).
-- |
-- | To get back to a `String`, there is a `Show` instance, so you can use `show`.
-- | However, that includes the `Int53` tag in the resulting string, so you might
-- | sometimes want [`toString`](#v:toString) instead.
-- |
-- | ### The `Int53Value` class
-- |
-- | There is also a class for [`Int53Value`](#t:Int53Value), which might sometimes be useful if you
-- | have a function where you'd like to accept either an `Int` or an `Int53`.
-- | Often you won't need this.

module Data.Int53
    ( Int53
    , class Int53Value, toInt53, fromInt53
    , fromNumber, toNumber
    , fromInt, toInt
    , fromString, toString
    , ceil, floor, round, truncate
    , even, odd
    , pow
    ) where


import Prelude
    ( class Semiring, add, zero, mul, one
    , class Ring, sub, class ModuloSemiring, div
    , class Bounded, top, bottom
    , class Eq, eq, class Ord, compare, class BoundedOrd
    , class Show, show
    , (==), ($), (++), (>), (<), negate, not, (<<<), (||), id
    )

import Math as Math
import Global (readFloat, isNaN)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Int (toNumber, floor) as Int
import Data.String (stripSuffix)


-- Internally, an Int53 is a newtype over a `Number`. We implement the various
-- arithmetic classes in a way that should guarantee that the `Number` is actually
-- an integer. Note that we can't expose the `Int53` constructor by itself, since
-- that would allow the construction of an `Int53` which isn't actually an integer.

-- | A 53-bit signed integer.
-- |
-- | It is implemented a newtype over a `Number` (that is, a Javascript number),
-- | with matters arranged so that it will always be an integer.
-- |
-- | In case you've forgotten what all the instances mean, you've got:
-- |
-- |     Semiring: add, zero, mul, one, (+), (*)
-- |     Ring: sub, (-), negate
-- |     ModuleSemiring: div, mod, (/)
-- |
-- | ... plus the usual `Eq`, `Ord` and `Show`.
-- |
-- | There's also `top` and `bottom` from `Bounded`, which indicate the maximum
-- | and minimum values available for an `Int53`.
newtype Int53 = Int53 Number


instance semiringInt53 :: Semiring Int53 where
    add (Int53 a) (Int53 b) = Int53 $ add a b
    zero = Int53 zero
    mul (Int53 a) (Int53 b) = Int53 $ mul a b
    one = Int53 one


instance ringInt53 :: Ring Int53 where
    sub (Int53 a) (Int53 b) = Int53 $ sub a b


-- This is particularly where we need to make sure we're actually
-- returning an integer.
--
-- Purescript is more aggressive than this with the `Int` type,
-- dropping fractional parts via `(i | 0)` even when you wouldn't
-- think a fractional part was possible.
instance moduloSemiringInt53 :: ModuloSemiring Int53 where
    div (Int53 a) (Int53 b) = truncate $ div a b
    mod (Int53 a) (Int53 b) = Int53 $ Math.(%) a b


instance eqInt53 :: Eq Int53 where
    eq (Int53 a) (Int53 b) = eq a b


instance ordInt53 :: Ord Int53 where
    compare (Int53 a) (Int53 b) = compare a b


instance boundedInt53 :: Bounded Int53 where
    top = Int53 topFloat
    bottom = Int53 bottomFloat


-- Basically, Number.MAX_SAFE_INTEGER
topFloat :: Number
topFloat = 9007199254740991.0


-- Basically, Number.MIN_SAFE_INTEGER
bottomFloat :: Number
bottomFloat = -9007199254740991.0


instance boundedOrdInt53 :: BoundedOrd Int53 where


-- Note that this produces valid PureScript (in one sense), but we
-- don't actually expose the `Int53` constructor, since that would
-- allow constructing an `Int53` which isn't really an integer.
-- So, I suppose we could use `truncate` here instead, but that would
-- seem a bit obscure, say, in an interactive repl session.
instance showInt53 :: Show Int53 where
    show (Int53 a) = "(Int53 " ++ show a ++ ")"


-- Clamps to the top and bottom. Unsafe because it assumes that
-- something has already been done to remove any fractional part.
unsafeClamp :: Number -> Int53
unsafeClamp a =
    if a > topFloat
        then top
        else
            if a < bottomFloat
                then bottom
                else Int53 a


-- | Convert a `Number` to an `Int53`, by rounding towards zero.
-- | Values outside the `Int53` range are clamped.
-- |
-- |     truncate 27.1 == fromInt 27
-- |     truncate 27.9 == fromInt 27
-- |     truncate (-27.1) == fromInt (-27)
-- |     truncate (-27.9) == fromInt (-27)
-- |     truncate (1.0e65) == top
-- |     truncate (-1.0e65) == bottom
truncate :: Number -> Int53
truncate a =
    if a > 0.0
        then floor a
        else ceil a


-- | Convert a `Number` to an `Int53`, by taking the closest integer equal to or
-- | less than the argument. Values outside the `Int53` range are clamped.
-- |
-- |     floor 27.1 == fromInt 27
-- |     floor 27.9 == fromInt 27
-- |     floor (-27.1) == fromInt (-28)
-- |     floor (-27.9) == fromInt (-28)
-- |     floor (1.0e65) == top
-- |     floor (-1.0e65) == bottom
floor :: Number -> Int53
floor = unsafeClamp <<< Math.floor


-- | Convert a `Number` to an `Int53`, by taking the closest integer equal to or
-- | greater than the argument. Values outside the `Int53` range are clamped.
-- |
-- |     ceil 27.1 == fromInt 28
-- |     ceil 27.9 == fromInt 28
-- |     ceil (-27.1) == fromInt (-27)
-- |     ceil (-27.9) == fromInt (-27)
-- |     ceil (1.0e65) == top
-- |     ceil (-1.0e65) == bottom
ceil :: Number -> Int53
ceil = unsafeClamp <<< Math.ceil


-- | Convert a `Number` to an `Int53`, by taking the nearest integer to the
-- | argument. Values outside the `Int53` range are clamped.
-- |
-- |     round 27.1 == fromInt 27
-- |     round 27.9 == fromInt 28
-- |     round (-27.1) == fromInt (-27)
-- |     round (-27.9) == fromInt (-28)
-- |     round (1.0e65) == top
-- |     round (-1.0e65) == bottom
round :: Number -> Int53
round = unsafeClamp <<< Math.round


-- | Creates an `Int53` from a `Number` value. The number must already be an
-- | integer and fall within the valid range of values for the `Int53` type.
-- | Otherwise, `Nothing` is returned.
-- |
-- |     fromNumber Global.nan == Nothing
-- |     fromNumber 2.5 == Nothing
-- |     fromNumber 1.0e65 == Nothing
-- |     fromNumber (-1.0e65) == Nothing
-- |     fromNumber 27.0 == Just (fromInt 27)
fromNumber :: Number -> Maybe Int53
fromNumber a =
    if isNaN a || a > topFloat || a < bottomFloat
        then Nothing
        else
            if Math.floor a == a
                then Just $ Int53 a
                else Nothing


-- | Converts an `Int53` value back into a `Number`. Any `Int53` is a valid `Number`
-- | so there is no loss of precision with this function.
toNumber :: Int53 -> Number
toNumber (Int53 a) = a


-- | Reads an `Int53` from a `String` value. The number must parse as an integer
-- | and fall within the valid range of values for the `Int53` type, otherwise
-- | `Nothing` is returned.
-- |
-- |     fromString "not a number" == Nothing
-- |     fromString "2.5" == Nothing
-- |     fromString "1.0e65" == Nothing
-- |     fromString "-1.0e65" == Nothing
-- |     fromString "27.0" == Just (fromInt 27)
-- |     fromString "27" == Just (fromInt 27)
fromString :: String -> Maybe Int53
fromString = fromNumber <<< readFloat


-- | Converts an `Int53` to a `String`.
-- |
-- | There is also a `Show` instance (so you can use `show`), but that includes
-- | the `Int53` tag in the string, which isn't always what you'll want.
-- |
-- |      toString (fromInt 27) == "27"
-- |      show (fromInt 27) == "(Int53 27.0)"
toString :: Int53 -> String
toString (Int53 a) =
    -- In principle, it should always end in ".0", but we'll check
    fromMaybe showNum (stripSuffix ".0" showNum)
      where
        showNum = show a


-- | Converts an `Int` to an `Int53`.
fromInt :: Int -> Int53
fromInt = Int53 <<< Int.toNumber


-- | Converts an `Int53` to an `Int`. Values outside the `Int` range are clamped.
-- |
-- |     toInt (fromInt 27) ==> 27
-- |     toInt (floor (1.0e52)) ==> 2147483647
-- |     toInt (floor (-1.0e52)) ==> (-2147483648)
toInt :: Int53 -> Int
toInt (Int53 a) =
    -- Calling `floor` gets us a cheap clamp ... of course, we shouldn't need the floor
    Int.floor a


-- | Returns whether an `Int53` is an even number.
-- |
-- |     even (fromInt 0) == true
-- |     even (fromInt 1) == false
even :: Int53 -> Boolean
even (Int53 a) = Math.(%) a 2.0 == 0.0


-- | The negation of `even`.
-- |
-- |     odd (fromInt 0) == false
-- |     odd (fromInt 1) == true
odd :: Int53 -> Boolean
odd = not <<< even


-- | Raises the first argument to the power of the second argument (the exponent).
-- |
-- | If the exponent is less than 0, then `pow` returns 0.
-- |
-- |     pow (fromInt 2) (fromInt 3) == (fromInt 8)
-- |     pow (fromInt 2) (fromInt 0) == (fromInt 1)
-- |     pow (fromInt 0) (fromInt 0) == (fromInt 1)
-- |     pow (fromInt 2) (fromInt (-2)) == (fromInt 0)
pow :: Int53 -> Int53 -> Int53
pow (Int53 base) (Int53 exponent) =
    if exponent < 0.0
        then Int53 0.0
        else Int53 $ Math.pow base exponent


-- | A class which allows a function to accept eitner an `Int` or an `Int53`,
-- | work with `Int53` internally, and then return whatever type was provided.
-- |
-- | For instance, you can do something like:
-- |
-- |     doSomethingWithIntOrInt53 :: ∀ a. (Int53Value a) => a -> a
-- |     doSomethingWithIntOrInt53 =
-- |         fromInt53 <<< doSometingWithInt53 <<< toInt53
-- |
-- |     doSomethingWithInt53 :: Int53 -> Int53
-- |     doSomethingWithInt53 = ...
-- |
-- | This is basically for cases where only some **intermediate** steps need the
-- | `Int53` range -- that is, where an input in the `Int` range will produce an
-- | output in the `Int` range, but one needs an `Int53` in the middle.
-- |
-- | So, you won't need this very often ... it's just a convenience in some cases.
class Int53Value a where
    toInt53 :: a -> Int53
    fromInt53 :: Int53 -> a


instance int53Int53Value :: Int53Value Int53 where
    toInt53 = id
    fromInt53 = id


instance intInt53Value :: Int53Value Int where
    toInt53 = fromInt
    fromInt53 = toInt
