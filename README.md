# what4-serialize

In-progress serialization tool for What4 expressions.

Building
--------

The dependencies of the project that are not on Hackage are specified using git submodules.  To build the code with a modern version of ``cabal`` (assuming you are in the root of the repository)::

  $ git submodule update --init
  $ ln -s cabal.project.newbuild cabal.project
  $ cabal new-configure
  $ cabal new-build what4-serialize
