# Getopts, analyse command line arguments for OCaml programs

The **Getopts** projects implements a library to functionally analyse
command line arguments in OCaml programs.

[![Build Status](https://travis-ci.org/michipili/getopts.svg?branch=master)](https://travis-ci.org/michipili/getopts?branch=master)

It support traditional short options, option clusters, and also long
options introduced by a special letter, similar to what **gcc** or
**clang** proposes.


## Option clusters

The **Getopts** module support option clusters, meaning that if `-a`
is a command flag and `-o` and option with an argument, all the
invocations

    cmd -aoarg file file
    cmd -a -o arg file file
    cmd -a -oarg file file

are interpreted the same way.


## Option delimiter

The **Getopts** module uses `--` as an option delimiter, meaning that
meaning that if `-a` is a command flag and `-o` and option with an
argument then the invocations

    cmd -a -oarg -- file file
    cmd -a -oarg file file

are equivalent.


## Options with long names

GNU style long options are not supported but it is possible to use
something similar to an option with a long name, in a similar way as
**gcc** or **clang** does. It is possible to configure **Getopts** so
that invocations like

    cmd -fsyntax-only -fobjc-abi-version=2 -March=hammer
    cmd -f syntax-only -f objc-abi-version=2 -M arch=hammer

are easily analysed.


## Help screen

The **Getopts** module can automatically prepare and format an help
screen, similar to this one:

    Usage: test_getopts [-avh][-i int][-f float][-b bool][-s string][--][rest]
     Test the Getopts module
    Options:
     -h Display available options.
     -a The a flag.
     -v The v flag.
     -b The b option.
     -c The c option.
     -s The s option.
     -i The i option.
     -f The f option.
    Short Note: This is an example of a short note.
    Long Note:
     This is an example of a very long note whose contents spans over several
     lines.  It really has to be that long, so we will write here as much silly
     text as needed.
       And we also need several pragraphs of irrelevant text, but believe me,
     this has nothing to do with you, just with the example.


## Free software

It is written by Michael Grünewald and is distributed as a free
software: copying it  and redistributing it is
very much welcome under conditions of the [CeCILL-B][licence-url]
licence agreement, found in the [COPYING][licence-en] and
[COPYING-FR][licence-fr] files of the distribution.


## Setup guide

It is easy to install **Getopts** using **opam** and its *pinning*
feature.  In a shell visiting the repository, say

```console
% opam pin add getopts .
```

It is also possible to install **Getopts** manually.
The installation procedure is based on the portable build system
[BSD Owl Scripts][bsdowl-home] written for BSD Make.

1. Verify that prerequisites are installed:
   - BSD Make
   - [BSD OWl][bsdowl-install]
   - OCaml
   - GNU Autoconf

2. Get the source, either by cloning the repository or by exploding a
   [distribution tarball](releases).

3. Optionally run `autoconf` to produce a configuration script. This
   is only required if the script is not already present.

4. Run `./configure`, you can choose the installation prefix with
   `--prefix`.

5. Run `make build`.

6. Optionally run `make test` to test your build.

7. Finally run `make install`.

Depending on how **BSD Make** is called on your system, you may need to
replace `make` by `bsdmake` or `bmake` in steps 5, 6, and 7.
The **GNU Make** program usually give up the ghost, croaking
`*** missing separator. Stop.` when you mistakingly use it instead of
**BSD Make**.

Step 7 requires that you can `su -` if you are not already `root`.


Michael Grünewald in Bonn, on October 21, 2014


  [licence-url]:        http://www.cecill.info/licences/Licence_CeCILL-B_V1-en.html
  [licence-en]:         COPYING
  [licence-fr]:         COPYING-FR
  [bsdowl-home]:        https://github.com/michipili/bsdowl
  [bsdowl-install]:     https://github.com/michipili/bsdowl/wiki/Install
  [mixture-home]:       https://github.com/michipili/mixture
  [mixture-test]:       https://github.com/michipili/mixture
