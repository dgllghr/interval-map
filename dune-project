(lang dune 2.0)
(name interval-tree)
(documentation "https://Dan.github.io/interval-tree/")
(source (github dgllghr/interval-tree))
(license MIT)
(authors "Dan Gallagher")
(maintainers "Dan Gallagher")

(generate_opam_files true)

(package
 (name interval-tree)
 (synopsis "A short, but powerful statement about your project")
 (description "\
A short, but powerful statement about your project
")
 (depends
  (ocaml (>= 4.08.0))
  (dune (>= 2.0))
  (alcotest :with-test)
  (odoc :with-doc)))
