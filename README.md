# purescript-int-53

Purescript's built-in `Int` type is restricted to 32-bit integers. However, the
Javascript runtime is capable of working with 53-bit integers. So, this module
provides an `Int53` type, for cases where you want all 53 bits
(from -9,007,199,254,740,991 to 9,007,199,254,740,991).

To install the dependencies, try `bower install`

To run the tests, try `pulp test`
