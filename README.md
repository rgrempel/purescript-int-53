[![Latest release](http://img.shields.io/bower/v/purescript-int-53.svg)](https://github.com/rgrempel/purescript-int-53/releases)
[![Dependency Status](https://www.versioneye.com/user/projects/57007272fcd19a0051853c1d/badge.svg?style=flat)](https://www.versioneye.com/user/projects/57007272fcd19a0051853c1d)
[![Build Status](https://travis-ci.org/rgrempel/purescript-int-53.svg?branch=master)](https://travis-ci.org/rgrempel/purescript-int-53)

# purescript-int-53

Purescript's built-in `Int` type is restricted to 32-bit integers. However, the
Javascript runtime is capable of working with 53-bit integers. So, this module
provides an `Int53` type, for cases where you want all 53 bits
(from -9,007,199,254,740,991 to 9,007,199,254,740,991).

## Installation

Try `bower install purescript-int-53`

## Development

Try something like:

    git clone https://github.com/rgrempel/purescript-int-53
    bower install
    pulp test

## API

Documentation for the API can be found on [Pursuit](https://pursuit.purescript.org/packages/purescript-int-53).
Or, if you are already looking at Pursuit, then below ...

