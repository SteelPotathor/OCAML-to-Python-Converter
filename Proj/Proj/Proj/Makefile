all: comp

# Compilation of Ocaml files
# Attention: order of object files important 
comp: lang.cmo parser.cmo lexer.cmo typing.cmo eval.cmo transf.cmo pprinter.cmo interf.cmo comp.cmo 
	ocamlfind ocamlc -linkpkg -package pprint -o comp $^

# Compilation of .ml files
lang.cmo: lang.ml
	ocamlc -c $<

transf.cmo: transf.ml lang.cmo
	ocamlc -c $<

typing.cmo: typing.ml lang.cmo
	ocamlc -c $<

eval.cmo: eval.ml lang.cmo
	ocamlc -c $<

interf.cmo: interf.ml parser.cmo lexer.cmo typing.cmo eval.cmo transf.cmo
	ocamlc -c $<

comp.cmo: comp.ml interf.cmo
	ocamlc -c $<

pprinter.cmo: pprinter.ml lang.cmo
	ocamlfind ocamlc -linkpkg lang.cmo -package pprint pprinter.ml

# ocaml lexer and parser: sources
# Comment in the following lines when lexer.mll and parer.mly are operational
# lexer.ml: lexer.mll lang.cmo
# 	ocamllex $<
# parser.ml parser.mli: parser.mly lang.cmo
# 	ocamlyacc -v $<

# ocaml lexer and parser: generated
lexer.cmo: lexer.ml parser.cmo
	ocamlc -c $<
parser.cmo: parser.ml lang.cmo
	ocamlc -c $<


#### Generic rules

%.cmi: %.mli
	ocamlc -c $<


.PHONY: clean

clean: 
	rm -f *.mli *.cmi *.cmo
