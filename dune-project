(lang dune 3.10)

(name rays)

(generate_opam_files true)

(source
 (github artempyanykh/rays))

(authors "Artem Pianykh")

(maintainers "Artem Pianykh")

(license MIT)

(documentation https://url/to/documentation)

(package
 (name rays)
 (synopsis "Simple ray tracer in OCaml")
 (description "An OCaml implementation of 'Ray Tracing in One Weekend'")
 (depends
  ocaml
  dune
  (ppx_expect :with-test))
 (tags
  ("ray tracing" graphics)))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/dune-files.html#dune-project
