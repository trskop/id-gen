id-gen
======


Description
-----------

Framework for generating pseudo-random IDs.

Basic idea of this library is to use simple monotonically increasing sequence,
and turn it into seemingly random ID. This is done by a composition of multiple
invertible functions, which as a whole is also invertible.
