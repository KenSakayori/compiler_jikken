(lang dune 3.22)
(name compiler_jikken)
(generate_opam_files)
(version v2026.1)
(source (github KenSakayori/compiler_jikken))
(authors "Ken Sakayori <sakayori@is.s.u-tokyo.ac.jp>")
(maintainers "Ken Sakayori <sakayori@is.s.u-tokyo.ac.jp>")

(package
 (name compiler_jikken)
 (synopsis "Checker & archiver for compiler jikken")
 (depends
  (ocaml (>= 5.4.1))
   ocamlfind
   dune
   batteries
   fileutils
))
