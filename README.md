cabal-bounds
============

A command line program for managing the bounds/versions of the dependencies in a cabal file.

`cabal-bounds` is able to do two things:
* drop the bounds of the dependencies in the cabal file
* update the bounds of the dependencies in the cabal file using the cabal build information

Example: Raise the upper Bounds
===============================

If you have several cabalized projects, then it can be quite time consuming to keep the
bounds of your dependencies up to date. Especially if you're following the [package versioning policy](<http://www.haskell.org/haskellwiki/Package_versioning_policy>),
then you want to raise your upper bounds from time to time, to allow the building with newer
versions of the dependencies.

`cabal-bounds` tries to automate this update process to some degree. So a typical update process might look like:

    # update the version infos of all libraries
    $> cabal update

    # drops the upper bound of all dependencies in 'myproject.cabal', most likely you want to ignore 'base'
    $> cabal-bounds drop --upper --ignore=base myproject.cabal

    # create a cabal sandbox for building of 'myproject'
    $> cabal sandbox init
      
    # build 'myproject'
    $> cabal install

    # update the upper bound of all dependencies in 'myproject.cabal' by the cabal build information
    $> cabal-bounds update --upper --ignore=base myproject.cabal dist/dist-sandbox-*/setup-config 

Example: Bound Changes
======================

The `=>` shows what the result is of the operation for every dependency. Left is the dependency before
calling the command, right the one after calling.

    $> cabal-bounds drop ...
    lens >=4.0.1 && <4.1   =>   lens

    $> cabal-bounds drop --upper ...
    lens >=4.0.1 && <4.1   =>   lens >=4.0.1

If the cabal build (the setup-config) uses `lens 4.1.2`, then the results of the `update` command would be:

    $> cabal-bounds update ...
    lens >=4.0.1 && <4.1   =>   lens >=4.1.2 && <4.2
    lens                   =>   lens >=4.1.2 && <4.2

    $> cabal-bounds update --lower ...
    lens >=4.0.1 && <5     =>   lens >=4.1.2 && <5
    lens >=4.0.1 && <4.1   =>   lens >=4.1.2
    lens <4.1              =>   lens >=4.1.2
    lens                   =>   lens >=4.1.2

    $> cabal-bounds update --upper ...
    lens >=4.0.1 && <4.1   =>   lens >=4.0.1 && <4.2
    lens >=4.0.1           =>   lens >=4.0.1 && <4.2
    lens                   =>   lens <4.2

You can also specify which component of the version number should be updated:

    $> cabal-bounds update --lowercomp=minor ...
    lens >=4.0.1 && <4.1   =>   lens >=4.1.2

    $> cabal-bounds update --lowercomp=major2 ...
    lens >=4.0.1 && <4.1   =>   lens >=4.1

    $> cabal-bounds update --lowercomp=major1 ...
    lens >=4.0.1 && <4.1   =>   lens >=4 && <4.1

    $> cabal-bounds update --uppercomp=minor ...
    lens >=4.0.1 && <4.1   =>   lens >=4.0.1 && <4.1.3

    $> cabal-bounds update --uppercomp=major2 ...
    lens >=4.0.1 && <4.1   =>   lens >=4.0.1 && <4.2

    $> cabal-bounds update --uppercomp=major1 ...
    lens >=4.0.1 && <4.1   =>   lens >=4.0.1 && <5

Options
=======

You can restrict the modification to certain sections in the cabal file by specifing the type and the name of the section:
* `--library`
* `--executable=name`
* `--testsuite=name`
* `--benchmark=name`

If you omit these options, then all sections are considered and modified.

You can also restrict the modification of dependencies by specifing which dependencies should only or shouldn't be modified:
* `--only=name`
* `--ignore=name`

If you omit these options, then all dependencies are considered and modified.

All options taking a name can be specified multiple times:
e.g. `--executable=exe1 --executable=exe2` or `--ignore=base --ignore=whatever`

Please consult `cabal-bounds --help` for a complete list of options.

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
