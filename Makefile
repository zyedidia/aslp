################################################################
# ASL Makefile
#
# Copyright Arm Limited (c) 2017-2019
# SPDX-Licence-Identifier: BSD-3-Clause
################################################################

.DEFAULT: all

VERSION = 0.1

OTT             := ott

BUILDFLAGS      += -use-ocamlfind
BUILDFLAGS      += -tag thread
BUILDFLAGS      += -tag debug
BUILDFLAGS      += -cflags -safe-string

ifeq ($(shell opam config var ott:share),\#undefined)
MENHIR_EXTRA    = $(OTT_DIR)/menhir/menhir_library_extra.mly
else
MENHIR_EXTRA    = `opam config var ott:share`/menhir_library_extra.mly
endif

MENHIRFLAGS     += --infer
MENHIRFLAGS     += --explain
MENHIR          := -menhir "menhir $(MENHIRFLAGS)"


SRCS += libASL/asl_ast.ml
SRCS += libASL/asl_parser.mly
SRCS += libASL/asl_parser_pp.ml
SRCS += libASL/elf.ml
SRCS += libASL/lexersupport.ml
SRCS += libASL/lexer.mll
SRCS += libASL/load_asl.ml
SRCS += libASL/tcheck.ml
SRCS += libASL/asl_utils.ml
SRCS += libASL/asl_visitor.ml
SRCS += libASL/utils.ml
SRCS += libASL/visitor.ml
SRCS += libASL/primops.ml
SRCS += libASL/value.ml
SRCS += libASL/eval.ml

SRCS += bin/asli.ml

all :: asli.native
asli.native: $(SRCS)
	echo Execute the following: export DYLD_LIBRARY_PATH=`opam config var z3:lib`
	ocamlbuild $(BUILDFLAGS) $(MENHIR) $@

bin/asli.byte: $(SRCS)
	echo Execute the following: export DYLD_LIBRARY_PATH=`opam config var z3:lib`
	ocamlbuild $(BUILDFLAGS) $(MENHIR) $@

doc :: asli.docdir/index.html
asli.docdir/index.html: $(SRCS)
	ocamlbuild -use-ocamlfind $@

all :: asli
asli: asli.native
	ln -f -s $^ asli

clean ::
	$(RM) bin/asli.byte asli.native asli
	$(RM) -r _build
	$(RM) asl.tex libASL/asl_ast.ml libASL/asl_parser.mly libASL/asl_lexer.mll libASL/asl_parser_pp.ml
	$(RM) -r asli.docdir
	ocamlbuild -clean

all :: testlexer.native

testlexer.native: bin/testlexer.ml libASL/lexersupport.ml libASL/lexer.mll
	# Adding Z3 to the dynamic library path would not be necessary if we made
	# use of the Z3 package conditional on what target we were building
	echo Execute the following: export DYLD_LIBRARY_PATH=`opam config var z3:lib`
	ocamlbuild $(BUILDFLAGS) $(MENHIR) $@


# generate the ocaml AST type, ocamllex lexer, menhir parser, and ocaml pretty printers for the AST, all from the Ott soruce
libASL/asl_ast.ml  libASL/asl_lexer.mll libASL/asl_parser.mly libASL/asl_parser_pp.ml libASL/asl_ast.tex : libASL/asl.ott
	(cd libASL; $(OTT) -aux_style_rules false -tex_wrap true -quotient_rules false -i asl.ott  -o asl_parser.mly -o asl_lexer.mll -o asl_ast.ml -o asl.tex)
	grep -v '^%%' $(MENHIR_EXTRA) >> libASL/asl_parser.mly

# We need a separate rule to build LaTeX so that it is unquotiented
# (despite the above specifying -quotient_rules false)
asl_grammar.tex: libASL/asl.ott
	grep -v spice libASL/asl.ott | grep -v '__builtin' | grep -v '__function' | grep -v '__ExceptionTaken' > asl_clean.ott
	libASL; $(OTT) -tex_wrap false -quotient_rules false -generate_aux_rules false -aux_style_rules false -i asl_clean.ott -o $@
	perl -p -i -e 's/{\\textsf{S}}/{}/' $@

clean ::
	$(RM) asl_grammar.tex asl_clean.ott

# all :: asl_quotiented.pdf
pdf: asl_quotiented.pdf asl_unquotiented.pdf

asl_quotiented.pdf: libASL/asl.ott Makefile
	$(OTT) -quotient_rules true -generate_aux_rules false -i libASL/asl.ott -o asl_quotiented.tex
	pdflatex asl_quotiented.tex

asl_unquotiented.pdf: libASL/asl.ott Makefile
	$(OTT) -quotient_rules false -generate_aux_rules false -aux_style_rules false -i libASL/asl.ott -o asl_unquotiented.tex
	pdflatex asl_unquotiented.tex

install::
	if [ -z "$(SHARE_DIR)" ]; then echo SHARE_DIR is unset; false; fi
	mkdir -p $(INSTALL_DIR)/bin
	cp asli.native $(INSTALL_DIR)/bin/asli

uninstall::
	if [ -z "$(SHARE_DIR)" ]; then echo SHARE_DIR is unset; false; else rm -rf $(SHARE_DIR); fi
	rm -f $(INSTALL_DIR)/bin/asli

publish::
	opam publish https://github.com/alastairreid/asl-interpreter/archive/$(VERSION).tar.gz

clean::
	$(RM) *~
	$(RM) asl_quotiented.{tex,pdf}
	$(RM) asl_unquotiented.{tex,pdf}
	$(RM) *.aux *.log *.bbl *.blg *.dvi *.out *.toc

################################################################
# End
################################################################
