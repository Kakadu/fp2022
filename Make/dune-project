(lang dune 2.8)

(generate_opam_files true)

(cram enable)

(license LGPL-3.0-or-later)

(authors "Ivan Morozko")

(maintainers "Ivan Morozko")

(bug_reports "https://github.com/ol-imorozko/fp2022")

(homepage "https://github.com/ol-imorozko/fp2022")

(package
 (name "Make")
 (synopsis "An interpreter for MAKE")
 (description "An interpreter for MAKE")
 (version 0.1)
 (depends
  dune
  angstrom
  (ppx_inline_test :with-test)
  qcheck-core
  ppx_expect
  ppx_deriving
  ppx_compare
  ppx_sexp_conv
  ppx_variants_conv
  ppx_hash
  graphlib
  core
  (odoc :with-doc)
  (ocamlformat :build)
  base
  ; After adding dependencies to 'dune' files and the same dependecies here too
  ))
