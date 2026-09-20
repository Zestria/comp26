(lang dune 3.24)

(name comp26)

(generate_opam_files true)

(using menhir 2.1)

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
  (llvm
   (= "18-shared"))
  (ppx_deriving_qcheck
   (= "0.6"))))

(package
 (name demo2)
 (allow_empty)
 (synopsis "A short synopsis")
 (description "A longer description")
 (depends ocaml)
 (tags
  ("add topics" "to describe" your project)))

(package
 (name vibecaml)
 (synopsis "Компилятор подмножества OCaml")
 (description
  "Учебный компилятор подмножества OCaml: два независимых синтаксических анализатора (нисходящий на комбинаторах и восходящий на menhir), вывод типов и кодогенерация под RISC-V.")
 (depends
  (ocaml
   (>= 5.5))
  dune
  (angstrom
   (>= 0.16.0))
  (menhir
   (>= 20231231))
  (qcheck
   (>= 0.21))
  (ppx_deriving
   (>= 5.2.1))
  bisect_ppx_ng))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/reference/dune-project/index.html
