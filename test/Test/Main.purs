module Test.Main where

import Data.Int53

import Data.Number (nan)
import Data.Int as Int
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Prelude (class Bounded, class CommutativeRing, class Eq, class EuclideanRing, class Ord, class Ring, class Semiring, class Show, Unit, add, bind, bottom, degree, discard, div, flip, mod, mul, negate, one, pure, show, sub, top, zero, ($), (*), (+), (-), (>))
import Test.QuickCheck ((===))
import Test.QuickCheck.Arbitrary (class Arbitrary)
import Test.QuickCheck.Gen (choose)
import Test.QuickCheck.Laws.Data.Bounded (checkBounded)
import Test.QuickCheck.Laws.Data.CommutativeRing (checkCommutativeRing)
import Test.QuickCheck.Laws.Data.Eq (checkEq)
import Test.QuickCheck.Laws.Data.EuclideanRing (checkEuclideanRing)
import Test.QuickCheck.Laws.Data.Ord (checkOrd)
import Test.QuickCheck.Laws.Data.Ring (checkRing)
import Test.QuickCheck.Laws.Data.Semiring (checkSemiring)
import Test.Unit (test)
import Test.Unit.Assert (equal)
import Test.Unit.Main (runTest)
import Test.Unit.QuickCheck (quickCheck)
import Type.Proxy (Proxy(..))


main :: Effect Unit

main = runTest do
    test "truncate" do
        truncate 27.1 ==> fromInt 27
        truncate 27.9 ==> fromInt 27
        truncate (-27.1) ==> fromInt (-27)
        truncate (-27.9) ==> fromInt (-27)
        truncate 0.0 ==> fromInt 0
        truncate 1.0e65 ==> top
        truncate (-1.0e65) ==> bottom

    test "floor" do
        floor 27.1 ==> fromInt 27
        floor 27.9 ==> fromInt 27
        floor (-27.1) ==> fromInt (-28)
        floor (-27.9) ==> fromInt (-28)
        floor 0.0 ==> fromInt 0
        floor 1.0e65 ==> top
        floor (-1.0e65) ==> bottom

    test "ceil" do
        ceil 27.1 ==> fromInt 28
        ceil 27.9 ==> fromInt 28
        ceil (-27.1) ==> fromInt (-27)
        ceil (-27.9) ==> fromInt (-27)
        ceil 0.0 ==> fromInt 0
        ceil 1.0e65 ==> top
        ceil (-1.0e65) ==> bottom

    test "round" do
        round 27.1 ==> fromInt 27
        round 27.9 ==> fromInt 28
        round (-27.1) ==> fromInt (-27)
        round (-27.9) ==> fromInt (-28)
        round 0.0 ==> fromInt 0
        round 1.0e65 ==> top
        round (-1.0e65) ==> bottom

    test "fromNumber" do
        fromNumber nan ==> nothing
        fromNumber 2.5 ==> nothing
        fromNumber 1.0e65 ==> nothing
        fromNumber (-1.0e65) ==> nothing
        fromNumber 27.0 ==> Just (fromInt 27)

    test "fromString" do
        fromString "not a number" ==> nothing
        fromString "2.5" ==> nothing
        fromString "1.0e65" ==> nothing
        fromString "-1.0e65" ==> nothing
        fromString "27.0" ==> Just (fromInt 27)
        fromString "27" ==> Just (fromInt 27)

    test "toString" do
        toString (fromInt 27) ==> "27"
        toString (fromInt (-27)) ==> "-27"
        toString (fromInt (0)) ==> "0"

    test "toInt" do
        toInt (fromInt 27) ==> 27
        toInt (floor (1.0e52)) ==> 2147483647
        toInt (floor (-1.0e52)) ==> (-2147483648)

    test "even" do
        even (fromInt 0) ==> true
        even (fromInt 1) ==> false
        even (fromInt 2) ==> true
        even (fromInt 3) ==> false

        quickCheck \a ->
            Int.even a === even (fromInt a)

    test "odd" do
        odd (fromInt 0) ==> false
        odd (fromInt 1) ==> true
        odd (fromInt 2) ==> false
        odd (fromInt 3) ==> true

        quickCheck \a ->
            Int.odd a === odd (fromInt a)

    test "pow" do
        pow (fromInt 2) (fromInt 3) ==> (fromInt 8)
        pow (fromInt 2) (fromInt 0) ==> (fromInt 1)
        pow (fromInt 0) (fromInt 0) ==> (fromInt 1)
        pow (fromInt 2) (fromInt (-2)) ==> (fromInt 0)

    test "abs" do
        abs (fromInt 2) ==> (fromInt 2)
        abs (fromInt (-2)) ==> (fromInt 2)
        abs (fromInt 0) ==> (fromInt 0)

    test "top" do
        ((topInt53 - one) + one) ==> topInt53
        (topInt53 + one) ==> topInt53
        (topInt53 * (one + one)) ==> topInt53
        (topInt53 - (negate one)) ==> topInt53
        (topInt53 `pow` (one + one)) ==> topInt53

        quickCheck \a ->
            (topInt53 - (posInt53 a)) + (posInt53 a) === topInt53

    test "bottom" do
        ((bottomInt53 + one) - one) ==> bottomInt53
        (bottomInt53 + (negate one)) ==> bottomInt53
        (bottomInt53 * (one + one)) ==> bottomInt53
        (bottomInt53 - one) ==> bottomInt53
        (bottomInt53 `pow` (one + one + one)) ==> bottomInt53

        quickCheck \a ->
            (bottomInt53 + (posInt53 a)) - (posInt53 a) === bottomInt53

    test "show" do
        show (fromInt 27) ==> "(Int53 27.0)"
        show (fromInt (-27)) ==> "(Int53 -27.0)"
        show (fromInt (0)) ==> "(Int53 0.0)"

    test "laws\n" $
        liftEffect do
            checkSemiring proxyInt53
            checkBounded proxyInt53
            checkRing proxyInt53
            checkCommutativeRing proxyInt53
            checkEuclideanRing proxyInt53
            checkEq proxyInt53
            checkOrd proxyInt53

infixl 9 equals as ==>

equals :: ∀ a. Eq a => Show a => a -> a -> Aff Unit
equals = flip equal


posInt53 :: Int -> Int53
posInt53 a =
    fromInt $
        if a > 0
            then a
            else (-a)


nothing :: Maybe Int53
nothing = Nothing


topInt53 :: Int53
topInt53 = top


bottomInt53 :: Int53
bottomInt53 = bottom


proxyInt53 :: Proxy Testable
proxyInt53 = Proxy


-- A newtype so that we can define an Arbitrary instance while
-- keeping Quickcheck in devDependencies rather than dependencies.
newtype Testable = Testable Int53

instance arbitraryTestable :: Arbitrary Testable where
    arbitrary = do
        -- This obviously isn't the full range of Int53.
        -- However, arbitraryInt does the same thing, and
        -- probably for the same reason ... which is to
        -- avoid quickcheck tests that overflow.
        -- TODO: Should adjust those tests, and then make
        -- this range bigger.
        n <- choose 94906265.0 (-94906265.0)
        pure $ Testable (truncate n)

-- Then, of course, we have to define all the instances
-- we're actually testing. Note that this is pretty generic ...
-- we could copy this elsewhere.
instance semiringTestable :: Semiring Testable where
    add (Testable a) (Testable b) = Testable $ add a b
    zero = Testable zero
    mul (Testable a) (Testable b) = Testable $ mul a b
    one = Testable one

derive instance eqTestable :: Eq Testable
derive instance ordTestable :: Ord Testable

instance boundedTestable :: Bounded Testable where
    top = Testable top
    bottom = Testable bottom

instance ringTestable :: Ring Testable where
    sub (Testable a) (Testable b) = Testable $ sub a b

instance euclideanRingTestable :: EuclideanRing Testable where
    degree (Testable a) = degree a
    div (Testable a) (Testable b) = Testable $ div a b
    mod (Testable a) (Testable b) = Testable $ mod a b

instance commutativeRingTestable :: CommutativeRing Testable
