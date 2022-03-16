ustring proposal repository
===========================

This repository contains code and documentation for an upcoming proposal regarding ustring, an immutable, unicode code point string class for C++.

The ustring.h file contains a complex implementation of the ustring specification described in ustring.md. The intention is that
other implementations can be made, with other trade-offs between complexity and simplicity and between storage usage and performance.

Some additional files are ustring_ni.md which contains some background text (quite old and not updated for recent changes), and
unicode_iterator which could be another proposal or maybe used as a component in the ustring implementation at a later stage. It is
more of a synopsis level at this point,

