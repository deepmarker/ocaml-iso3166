(lang dune 3.16)

(name iso3166)

(generate_opam_files true)

(source
 (github deepmarker/ocaml-iso3166))

(authors "Vincent Bernardoff <vb@luminar.eu.org>")

(maintainers "Vincent Bernardoff")

(license ISC)

(documentation https://url/to/documentation)

(package
 (name iso3166)
 (synopsis "ISO3166 country codes")
 (description "Codes for the representation of names of countries and their subdivisions")
 (depends
   ocaml
   dune
  (ezjsonm (and :build (>= 1.3.0)))
  (ocplib-json-typed (and :build (>= 0.7.1)))
  (pla (and :build (>= 2.0)))
  (sexplib (>= v0.17.0))
  (ppx_sexp_conv (>= v0.17.0))
 )
 (tags (iso iso3166 country code)))
