(lang dune 3.24)

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
  (llvm
   (= "18-shared"))
  ppx_deriving_qcheck))

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

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/reference/dune-project/index.html
