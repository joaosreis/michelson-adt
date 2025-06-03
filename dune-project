(lang dune 2.7)

(name michelson-adt)

(generate_opam_files true)

(source
 (uri "git+https://github.com/joaosreis/michelson-adt.git"))

(bug_reports "https://github.com/joaosreis/michelson-adt/issues")

(homepage "https://github.com/joaosreis/michelson-adt")

(license MIT)

(authors "Jo\195\163o Santos Reis <joaosreis@outlook.pt>")

(maintainers "Jo\195\163o Santos Reis <joaosreis@outlook.pt>")

(package
 (name michelson-adt)
 (version 0.1)
 (synopsis
  "An OCaml library with and ADT representation of Michelson smart contracts syntax")
 (depends
  (dune :build)
  (containers
   (and
    (>= 3.0)
    (< 4.0)))
  (zarith
   (and
    (>= 1.0)
    (< 2.0)))
  ppx_deriving
  pp_loc
  (alcotest :with-test)))
