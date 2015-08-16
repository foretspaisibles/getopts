### Makefile -- Test the Getopts module

# Author: Michael Grünewald
# Date: Sun Aug 10 14:47:34 CEST 2014

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

PROGRAM=		test_getopts

SRCS+=			main.ml

TESTS=			test-help-screen
TESTS+=			test-cluster
TESTS+=			test-terminator
TESTS+=			test-bool
TESTS+=			test-int
TESTS+=			test-char
TESTS+=			test-string
TESTS+=			test-rest
TESTS+=			test-float

ARGS.test-help-screen=	-h
ARGS.test-cluster=	-avava -avsva
ARGS.test-terminator=	-a -s -- -- -a
ARGS.test-bool=		-b true -b false
ARGS.test-int=		-i 42
ARGS.test-char=		-c X
ARGS.test-string=	-s text
ARGS.test-float=	-f 0.1
ARGS.test-rest=		rest1 rest2


.SUFFIXES: .expected .got


install:
	${NOP}

.for test in ${TESTS}
${test}.got: ${PROGRAM}
	${PROGRAM:tA} ${ARGS.${test}} > ${.TARGET}

CLEANFILES+=		${test}.got

test: do-${test}

.if !target(do-${test})
do-${test}: ${test}.expected ${test}.got
	diff -u ${.ALLSRC:M*.expected} ${.ALLSRC:M*.got}
.endif
.endfor

.include "ocaml.prog.mk"

### End of file `Makefile'
