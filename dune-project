(lang dune 3.24)

(using menhir 3.0)
(name comp26)

(generate_opam_files true)

(using menhir 3.0)

(source
 (github username/reponame))

(authors "Author Name <author@example.com>")

(maintainers "Maintainer Name <maintainer@example.com>")

(license LICENSE)

(documentation https://url/to/documentation)

(package
 (name demo)
 (synopsis "A short LLVM demo")
 (depends
  ocaml
  dune
  (angstrom
   (= "0.16.0"))
  qcheck
  bisect_ppx_ng
  ppx_deriving_qcheck))

(package
 (name shmel)
 (synopsis "A short LLVM demo")
 (depends
  ocaml
  dune
  (angstrom
   (= "0.16.0"))
  qcheck
  bisect_ppx_ng
  (llvm
   (= "18-shared"))
  (ppx_deriving_qcheck
   (= "0.9"))
  menhir))

(package
 (name SchonfinkelML)
 (synopsis "A small compiler")
 (depends
  ocaml
  dune
  menhir
  benchmark
  qcheck
  bisect_ppx_ng
  ppx_inline_test
  (llvm
   (= "18-shared"))
  (ppx_deriving_qcheck
   (= "0.9"))))

(package
 (name BDSMl)
 (synopsis "A small compiler")
 (depends
  ocaml
  dune
  (menhir :build)
  benchmark
  base
  (ppx_expect :with-test)
  (alcotest :with-test)
  (angstrom
   (= "0.16.0"))
  qcheck
  bisect_ppx_ng
  (llvm
   (= "18-shared"))
  (ppx_deriving_qcheck
   (= "0.9"))))

(package
 (name MMML)
 (synopsis "A short LLVM project")
 (depends
  ocaml
  dune
  menhir
  (angstrom
   (= "0.16.0"))
  qcheck
  bisect_ppx_ng
  ppx_deriving
  (llvm
   (= "18-shared"))
  (ppx_deriving_qcheck
   (>= "0.9"))))

(package
 (name vibecaml)
 (synopsis "Компилятор подмножества OCaml")
 (description
  "Учебный компилятор подмножества OCaml: два независимых синтаксических анализатора (нисходящий рекурсивным спуском и восходящий на menhir), вывод типов и кодогенерация под RISC-V.")
 (depends
  (ocaml
   (>= 5.5))
  dune
  (menhir
   (>= 20231231))
  (qcheck
   (and
    (>= 0.21)
    :with-test))
  (ppx_deriving
   (>= 5.2.1))
  (bisect_ppx_ng :with-test)))

(package
 (name BogdanKharisovPolMuzhskoyGorodNizhnevartovskML)
 (synopsis "OCaml subset compiler")
 (description "Compiler for a subset of OCaml.")
 (authors "shrexer06")
 (maintainers "shrexer06")
 (license LGPL-3.0-or-later)
 (source
  (uri "git+ssh://ssh.sourcecraft.dev/shrexer06/comp26.git"))
 (homepage "https://sourcecraft.dev/shrexer06/comp26")
 (documentation
  "https://sourcecraft.dev/shrexer06/comp26/browse/BogdanKharisovPolMuzhskoyGorodNizhnevartovskML/README.md?rev=frontend"))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/reference/dune-project/index.html
