id-gen
======

[![Haskell Programming Language](https://img.shields.io/badge/language-Haskell-blue.svg)](http://www.haskell.org)
[![BSD3 License](http://img.shields.io/badge/license-BSD3-brightgreen.svg)](https://tldrlegal.com/license/bsd-3-clause-license-%28revised%29)


Description
-----------

Framework for generating pseudo-random IDs.

Basic idea of this library is to use simple monotonically increasing sequence,
and turn it into seemingly random ID. This is done by a composition of multiple
invertible functions, which as a whole is also invertible.
