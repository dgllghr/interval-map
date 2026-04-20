(lang dune 3.0)
(name interval-map)
(version 0.2.1)
(documentation "https://dgllghr.github.io/interval-map/")
(source (github dgllghr/interval-map))
(license MIT)
(authors "Daniel Gallagher")
(maintainers "Daniel Gallagher")

(generate_opam_files true)

(package
 (name interval-map)
 (synopsis "An immutable interval map data structure")
 (description "\
An immutable interval map data structure implemented as an interval tree. Based
on [jgblight/im_interval_tree](https://github.com/jgblight/im_interval_tree).

Interval maps are great for finding intervals and their associated values which
overlap a given interval. This interval map supports intervals with excluded,
included, and unbounded bound ends. Multiple values may be associated with the
same interval.
")
 (depends
  (ocaml (>= 4.08.0))
  (alcotest (and (>= 1.4.0) :with-test))
  (odoc :with-doc)))
