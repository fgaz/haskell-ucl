# ucl

[![Hackage](https://img.shields.io/hackage/v/ucl.svg)](https://hackage.haskell.org/package/ucl)

**Datatype and parser for the Universal Configuration Language (UCL) using libucl**

The Universal Configuration Language (UCL) is a configuration language
inspired by nginx configuration files and compatible with JSON.
For a complete description of the language, see [the libucl readme](https://github.com/vstakhov/libucl/blob/master/README.md).

This library contains a datatype representing UCL objects, and a parser.
It is based on the C library [libucl](https://github.com/vstakhov/libucl),
which is needed to build this package.
