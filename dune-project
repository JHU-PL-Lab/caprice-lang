(lang dune 3.24)

(name caprice)

(using menhir 3.0)

(generate_opam_files true)

(license MIT)

(authors "Anonymous authors")

(maintainers "Anonymous maintainers")

(package
 (name caprice)
 (synopsis "The Caprice language")
 (description "Semantic type refutation via concolic semantics")
 (depends
  alcotest
  baby
  benchmark
  cmdliner
  menhir
  mtime
  (ocaml (>= 5.5.0))
  psq
  yojson
  z3
  ))
