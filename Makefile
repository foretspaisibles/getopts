### Makefile -- Getopts

# Author: Michael Grünewald
# Date: Tue Nov  5 22:37:27 CET 2013

# Getopts (https://github.com/michipili/getopts)
# This file is part of Getopts
#
# Copyright © 2008-2015 Michael Grünewald
#
# This file must be used under the terms of the CeCILL-B.
# This source file is licensed as described in the file COPYING, which
# you should have received as part of this distribution. The terms
# are also available at
# http://www.cecill.info/licences/Licence_CeCILL-B_V1-en.txt

PACKAGE=		getopts
VERSION=		0.3.2-releng
OFFICER=		michipili@gmail.com

MODULE=			ocaml.lib:src
MODULE+=		ocaml.meta:meta
MODULE+=		ocaml.manual:manual

SUBDIR=			testsuite

EXTERNAL=		ocaml.findlib:broken
EXTERNAL+=		ocaml.findlib:lemonade

CONFIGURE=		meta/getopts.in
CONFIGURE+=		Makefile.config.in

.include "generic.project.mk"

### End of file `Makefile'
