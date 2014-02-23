cabal-bounds
============

A command line program for managing the bounds/versions of the dependencies in a cabal file.

If you have several cabalized projects, then it can be quite time consuming to keep the
bounds of your dependencies up to date. Especially if you're following the [package versioning policy](<http://www.haskell.org/haskellwiki/Package_versioning_policy>),
then you want to raise your upper bounds from time to time, to allow the building with newer
versions of the dependencies.

`cabal-bounds` tries to automate this update process to some degree. So a typical update process might look like:

    $> # update the version infos of all libraries
    $> cabal update

    $> # drops the upper bound of all dependencies in 'myproject.cabal', most likely you want to ignore 'base'
    $> cabal-bounds drop --upper --ignore=base myproject.cabal

    $> # create a cabal sandbox for building of 'myproject'
    $> cabal sandbox init
      
    $> # build 'myproject'
    $> cabal install

    $> # update the upper bound of all dependencies in 'myproject.cabal' by the cabal build information
    $> cabal-bounds update --upper --ignore=base myproject.cabal dist/dist-sandbox-*/setup-config 

Installation
============

You have to ensure, that the `Cabal` library of `cabal-bounds` matches the one used by the `cabal` binary:

    $> cabal --version
    cabal-install version 1.18.0.2
    using version 1.18.1 of the Cabal library 

    $> cabal install --constraint="Cabal == 1.18.1" cabal-bounds

If you update the `cabal` binary and the used `Cabal` library changes, then you have to rebuild `cabal-bounds`.

Issues
======

Perhaps the currently most annoying thing is, that you have to live with the reformating of your
`cabal` file done by the pretty printer of the `Cabal` library.

To reformat your `cabal` file without changing any bounds you can call `cabal-bounds` with the name of
a section that isn't present in the `cabal` file:

    $> cabal-bounds drop --executable=blub myproject.cabal
