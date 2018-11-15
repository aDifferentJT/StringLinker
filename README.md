# StringLinker

StringLinker is a tool for linking large amounts of data with compiled programs.
By default it generates a C/C++ header to make it easy to use with C or C++.
It supports linux (tested with C and C++ using Clang on Debian, GCC on Debian and GCC on fedora), macOS (C and C++ using Clang and GCC) and Windows.

## How to use
1. Install [GHC](https://www.haskell.org)
2. Install [NASM](https://www.nasm.us)
   - Ensure that nasm is visible by StringLinker by adding it to your path, this ought to be done automatically by the installer but isn't on Windows.
3. Compile stringLinker.hs with GHC by running `ghc stringLinker.hs`.
4. [Optional] Put the stringLinker executable somewhere in your path.
