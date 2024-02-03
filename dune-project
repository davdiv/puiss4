(lang dune 3.13)

(name puiss4)

(generate_opam_files true)

(source
 (github davdiv/puiss4))

(authors "DivDE <divde@musicociel.fr>")

(maintainers "DivDE <divde@musicociel.fr>")

(license MIT)

(package
 (name puiss4)
 (synopsis "Puissance 4")
 (description "Jeu de Puissance 4")
 (depends ocaml dune graphics js_of_ocaml js_of_ocaml-ppx js_of_ocaml-lwt)
 (tags
  (topics "Puissance 4")))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/dune-files.html#dune-project
