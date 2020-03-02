# Makefile to build all standalone programs.
#

# COMMON=ast.sml machine.sml


# Default rules for lex and yacc
#
#
# This rule says that all `.lex.sml` files depend on the corresponding
# `.lex and they can be achieved by running mllex on $<.
%.lex.sml: %.lex
	mllex $<

# Similar to lex

%.grm.sml: %.grm
	mlyacc $<


all: tigc

PHONY: all clean 
tigc:  tiger.mlb tiger.lex.sml tiger.grm.sig tiger.grm.sml tiger.sml ast.sml beautify.sml  
	mlton -output tigc tiger.mlb
clean:
	echo "cleaning"
	rm -f *.lex.sml rp
	rm -f *.grm.sml *.grm.desc *.grm.sig ec
	rm -f tigc
	

# rp: rp.lex.sml rp.mlb rp.sml ${COMMON}
#	mlton rp.mlb



 test: all
	@echo "Nothing is there"
	${CURDIR}/rp test.inp
	${CURDIR}/ec test.expr | ${CURDIR}/rp

