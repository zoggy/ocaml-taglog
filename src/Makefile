#################################################################################
#                Taglog                                                         #
#                                                                               #
#    Copyright (C) 2015 INRIA. All rights reserved.                             #
#                                                                               #
#    This program is free software; you can redistribute it and/or modify       #
#    it under the terms of the GNU Lesser General Public License as             #
#    published by the Free Software Foundation, version 3 of the License.       #
#                                                                               #
#    This program is distributed in the hope that it will be useful,            #
#    but WITHOUT ANY WARRANTY; without even the implied warranty of             #
#    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the               #
#    GNU Library General Public License for more details.                       #
#                                                                               #
#    You should have received a copy of the GNU Lesser General Public           #
#    License along with this program; if not, write to the Free Software        #
#    Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA                   #
#    02111-1307  USA                                                            #
#                                                                               #
#    As a special exception, you have permission to link this program           #
#    with the OCaml compiler and distribute executables, as long as you         #
#    follow the requirements of the GNU GPL in regard to all of the             #
#    software in the executable aside from the OCaml compiler.                  #
#                                                                               #
#    Contact: Maxence.Guesdon@inria.fr                                          #
#                                                                               #
#################################################################################

include ../master.Makefile

OF_FLAGS=-package $(PACKAGES)

COMPFLAGS=-I +ocamldoc -g -verbose -annot #-w +K
OCAMLPP=

RM=rm -f
CP=cp -f
MKDIR=mkdir -p

# Base

LIB=taglog.cmxa
LIB_A=$(LIB:.cmxa=.a)
LIB_CMXS=$(LIB:.cmxa=.cmxs)
LIB_BYTE=$(LIB:.cmxa=.cma)

LIB_CMXFILES= \
	taglog.cmx

LIB_CMOFILES=$(LIB_CMXFILES:.cmx=.cmo)
LIB_CMIFILES=$(LIB_CMXFILES:.cmx=.cmi)
LIB_OFILES=$(LIB_CMXFILES:.cmx=.o)


#

all: opt byte

opt: $(LIB) $(LIB_CMXS)
byte: $(LIB_BYTE)

# Base

$(LIB): $(LIB_CMIFILES) $(LIB_CMXFILES)
	$(OCAMLFIND) ocamlopt -g -a -o $@ $(LIB_CMXFILES)

$(LIB_CMXS): $(LIB_CMIFILES) $(LIB_CMXFILES)
	$(OCAMLFIND) ocamlopt -g -shared -o $@ $(LIB_CMXFILES)

$(LIB_BYTE): $(LIB_CMIFILES) $(LIB_CMOFILES)
	$(OCAMLFIND) ocamlc -g -a -o $@ $(LIB_CMOFILES)

# Test
TEST1=test1

test: test1

$(TEST1): $(LIB) test_taglog.ml
	$(OCAMLFIND) ocamlopt -verbose -o $@ -linkpkg -package ocf \
	$(LIB) test_taglog.ml
	./$(TEST1)

.PHONY: $(TEST1)

##########
.PHONY: doc webdoc ocamldoc

doc: dump.odoc
	$(MKDIR) ocamldoc
	$(OCAMLFIND) ocamldoc -d ocamldoc -html $(OF_FLAGS) -verbose \
	-load dump.odoc -sort -t Taglog

docstog: dump.odoc
	$(MKDIR) ../web/refdoc
	ocamldoc.opt -t "Taglog reference documentation" \
	-load $^ -d ../web/refdoc \
	-i `$(OCAMLFIND) query stog` -g odoc_stog.cmxs
	rpl 'VAL(&&)' 'VAL(&amp;amp;&amp;amp;)' ../web/refdoc/*html

dump.odoc: taglog.mli
	$(OCAMLFIND) ocamldoc -v $(OF_FLAGS) taglog.mli -dump $@


##########
install: install-lib

install-lib:
	@$(OCAMLFIND) install $(PACKAGE) META \
		$(LIB_CMIFILES) $(LIB_CMXFILES) $(LIB_OFILES) \
		$(LIB_BYTE) $(LIB) $(LIB_A) $(LIB_CMXS) \
		$(LIB_CMIFILES:.cmi=.mli)

uninstall: uninstall-lib

uninstall-lib:
	@$(OCAMLFIND) remove $(PACKAGE)

#####
clean:
	$(RM) *.cm* *.o *.a *.x *.annot dump.odoc

distclean: clean

#############
.PRECIOUS:

.PHONY: clean depend

.depend depend:
	$(OCAMLFIND) ocamldep \
	`ls tag*.mli taglog*.ml `  > .depend

include .depend

############

