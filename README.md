cabal-bounds
============

A command line program for managing the bounds/versions of the dependencies in a cabal file.

`cabal-bounds` is able to do these things with the bounds of the dependencies in the cabal file:
* drop them
* update them by the library versions of the current cabal build
* update them by the library versions of a haskell platform release
* update them by the library versions specified by a file
* dump the libraries/dependencies and their lower bound versions from the cabal file(s) into a file

Example: Initialize Bounds
==========================

If you have started a new project, created a cabal file, added dependencies to it,
build it, and now want to set the lower and upper bounds of the dependencies
according to the currently used versions of the build, then you can just call:

    $> cabal-bounds update

This call will update the bounds of the dependencies of the cabal file in the working directory.

Example: Raise the Upper Bounds
===============================

If you have several cabalized projects, then it can be quite time consuming to keep the
bounds of your dependencies up to date. Especially if you're following the [package versioning policy](<http://www.haskell.org/haskellwiki/Package_versioning_policy>),
then you want to raise your upper bounds from time to time, to allow the building with newer
versions of the dependencies.

`cabal-bounds` tries to automate this update process to some degree. So a typical update process might look like:

    # update the version infos of all libraries
    $> cabal update

    # create a cabal sandbox for building your project, this ensures that you're really using
    # the newest available versions of the dependencies, otherwise you would be constraint
    # to the already installed versions
    $> cabal sandbox init
      
    # build your project and allow newer library versions used as specified by the cabal file.
    $> cabal install --allow-newer

    # update the upper bound of all dependencies of the cabal file in the working directory
    # if their upper bound is lower then the version used by the build
    $> cabal-bounds update --upper

If you're leaving out the `--upper` flag then `update` will widen the bounds on both directions.
So the upper bound will only be updated it it's lower and the lower bound only if it's greater.

Example: Update Bounds by Haskell Platform
==========================================

Ensuring that your project builds with the current [haskell platform](<https://www.haskell.org/platform/>) - or
perhaps the last two ones - can make it, especially for beginners, a lot easier to build your project.

`cabal-bounds` supports the updating of the bounds by the library versions of a specific haskell platform release.

To update the bounds to the haskell platform `2013.2.0.0`:

    $> cabal-bounds update --haskell-platform=2013.2.0.0

There're two additional symbolic names for specifying a haskell platform release: `current` and `previous`.

So one use case might be to initialize the bounds to library versions used by a haskell platform release,
test if your project builds and works with these, and then raise the upper bounds to the newest available versions:

    # intialize the bounds to the previous haskell platform release
    $> cabal-bounds update --haskell-platform=previous

    # build and test the project

    # initialize the bounds not present in the haskell platform
    $> cabal-bounds update

Example: Update Bounds by File
==============================

It's also possible to update the bounds by library versions specified in a file:

    $> cabal-bounds update --fromfile=libs.hs

The `libs.hs` file has to be of the format:

    [ ("libA", [0,2,1]), ("libB", [2,1]), ("libC", [1]) ]

If you specify a library file and a haskell platform release at once, then first the
haskell platform libraries and versions are considered and then the library file.

The library file can be created by the `dump` command:

    $> cabal-bounds dump --output=libs.hs

The `dump` command will dump dependencies with their lower bound version. The command can take multiple cabal files.
If the same dependencies is present in multiple files, then the lowest lower bound version is taken.

Example: Bound Changes
======================

The `=>` shows what the result is of the operation for every dependency. Left is the dependency before
calling the command, right the one after calling.

    $> cabal-bounds drop
    lens >=4.0.1 && <4.1   =>   lens

    $> cabal-bounds drop --upper
    lens >=4.0.1 && <4.1   =>   lens >=4.0.1

If the cabal build (the setup-config) uses `lens 4.1.2`, then the results of the `update` command would be:

    $> cabal-bounds update
    lens >=4.0.1 && <4.1   =>   lens >=4.0.1 && <4.2
    lens                   =>   lens >=4.1.2 && <4.2

    $> cabal-bounds update --lower
    lens >=4.0.1 && <5     =>   lens >=4.0.1 && <5
    lens >=4.0.1 && <4.1   =>   lens >=4.0.1 && <4.1
    lens <4.1              =>   lens >=4.1.2
    lens                   =>   lens >=4.1.2

    $> cabal-bounds update --upper
    lens >=4.0.1 && <4.1   =>   lens >=4.0.1 && <4.2
    lens >=4.0.1           =>   lens >=4.0.1 && <4.2
    lens                   =>   lens <4.2

You can also specify which component of the version number should be updated:

    $> cabal-bounds update --lowercomp=major1
    lens >=4.0.1 && <4.1   =>   lens >=4 && <4.1

    $> cabal-bounds update --uppercomp=minor
    lens >=4.0.1 && <4.1   =>   lens >=4.0.1 && <4.1.3

    $> cabal-bounds update --uppercomp=major2
    lens >=4.0.1 && <4.1   =>   lens >=4.0.1 && <4.2

    $> cabal-bounds update --uppercomp=major1
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

You can also only update the dependencies without a bound by specifying the `--missing` flag.
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

    $> cabal-bounds drop --executable=blub
