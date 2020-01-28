################################################################
# ASL Makefile
#
# Copyright Arm Limited (c) 2017-2019
# SPDX-Licence-Identifier: BSD-3-Clause
################################################################

.DEFAULT: all

VERSION = 0.1.1

OTT := ott

MENHIR_EXTRA    = `opam config var ott:share`/menhir_library_extra.mly

# generate the ocaml AST type, ocamllex lexer, menhir parser, and ocaml pretty printers for the AST, all from the Ott soruce
libASL/asl_ast.ml libASL/asl_lexer.mll libASL/asl_parser.mly libASL/asl_parser_pp.ml : libASL/asl.ott
	(cd libASL; $(OTT) -aux_style_rules false -tex_wrap true -quotient_rules false -i asl.ott  -o asl_parser.mly -o asl_lexer.mll -o asl_ast.ml)
	grep -v '^%%' $(MENHIR_EXTRA) >> libASL/asl_parser.mly

build:: libASL/asl_ast.ml
	dune build

install:: libASL/asl_ast.ml
	dune build @install
	dune install

uninstall:: libASL/asl_ast.ml
	dune build @install
	dune uninstall

publish:: libASL/asl_ast.ml
	dune build @install
	opam publish https://github.com/alastairreid/asl-interpreter/archive/$(VERSION).tar.gz

doc:: libASL/asl_ast.ml
	dune build @doc-private
	@echo Documentation is in _build/default/_doc/_html/libASL*/LibASL/index.html

clean::
	$(RM) *~
	$(RM) libASL/asl_ast.ml libASL/asl_lexer.ml libASL/asl_parser.ml libASL/asl_parser_pp.ml
	dune clean

################################################################
# End
################################################################
