MKSTOG=mk-stog
MKSTOG_OCAML_SESSION=mk-stog-ocaml-session
STOG=./mystog
STOG_OCAML_SESSION=./mystog-ocaml-session
STOG_SERVER=./mystog-server
DEST_DIR=../../taglog-gh-pages
BASE_URL_OPTION=
PACKAGES=stog.markdown,stog-writing
STOG_OPTIONS=--stog-ocaml-session "$(STOG_OCAML_SESSION) -ppx 'ocamlfind ppx_deriving/ppx_deriving `ocamlfind query -predicates ppx_driver,byte -a-format ppx_deriving_yojson`' -package ppx_deriving_yojson.runtime,yojson,ocf -I ../src" -d $(DEST_DIR) $(BASE_URL_OPTION)

build:
	$(MAKE) site

site: stog
	$(STOG) $(STOG_OPTIONS) .

ocamldoc:
	rm -f refdoc/*html ref-doc/style.css
	(cd .. && make docstog)

test:
	$(MAKE) BASE_URL_OPTION="--local" site

stog:$(STOG) $(STOG_OCAML_SESSION)
server: $(STOG_SERVER) $(STOG_OCAML_SESSION)
	$(STOG_SERVER) $(STOG_OPTIONS) .

.PHONY:$(STOG) $(STOG_OCAML_SESSION)

$(STOG):
	$(MKSTOG) -o $@ -package $(PACKAGES) -linkall

$(STOG_SERVER):
	$(MKSTOG) -o $@ -package stog.server,$(PACKAGES) -thread -linkall

$(STOG_OCAML_SESSION):
	$(MKSTOG_OCAML_SESSION) -o $@ -I ../src ../src/taglog.cma \
	-package ppx_deriving_yojson,ocf

clean:
	rm -fr .stog/cache

cleanall: clean
	rm -f $(STOG) $(STOG_OCAML_SESSION) $(STOG_SERVER)


