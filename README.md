# what4-serialize

This library is **deprecated** because its functionality has been merged into the core what4 library.

This library provided a mechanism to serialize what4 terms into an s-expression format (and later deserialize them).

# Building

The dependencies of the project that are not on Hackage are specified using git submodules.  To build the code with a modern version of ``cabal`` (assuming you are in the root of the repository):

```
  $ git submodule update --init
  $ ln -s cabal.project.newbuild cabal.project
  $ cabal new-configure
  $ cabal new-build what4-serialize
```
