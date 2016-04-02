# purescript-int-53

Purescript's built-in `Int` type is restricted to 32-bit integers. However, the
Javascript runtime is capable of working with 53-bit integers. So, this module
provides an `Int53` type, for cases where you want all 53 bits
(from -9,007,199,254,740,991 to 9,007,199,254,740,991).

## API

Documentation for the API can be found on [Pursuit](https://pursuit.purescript.org/packages/purescript-int-53).

## Installation

Try `bower install purescript-int-53`

## Development

Try something like:

    git clone https://github.com/rgrempel/purescript-int-53
    bower install
    pulp test
