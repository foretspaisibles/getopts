### Makefile -- Test suite

# Author: Michael Grünewald
# Date: Mon Apr 14 23:52:37 CEST 2014

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

PROGRAM=	unit_testing

SRCS+=		main.ml

OCAMLLFLAGS+=	-linkall

test: ${PROGRAM}
	${PROGRAM:tA}

install:
	${NOP}

.include "ocaml.prog.mk"

### End of file `Makefile'
