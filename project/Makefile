# Makefile to build all standalone programs.
#

# COMMON=ast.sml machine.sml


# Default rules for lex and yacc
#
#
# This rule says that all `.lex.sml` files depend on the corresponding
# `.lex and they can be achieved by running mllex on $<.
# %.lex.sml: %.lex
#	mllex $<

# Similar to lex

# %.grm.sml: %.grm
#	mlyacc $<


all: tiger

PHONY: all clean test

clean:
	rm -f tiger

# rp: rp.lex.sml rp.mlb rp.sml ${COMMON}
#	mlton rp.mlb

tiger: tiger.sml tiger.mlb 
	mlton tiger.mlb


 test: all
	${CURDIR}/rp test.inp
	${CURDIR}/ec test.expr | ${CURDIR}/rp

