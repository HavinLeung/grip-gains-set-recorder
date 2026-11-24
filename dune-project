(lang dune 3.21)

(using melange 0.1)

(name grip_gains_set_recorder)

; .mlx = OCaml + JSX (https://github.com/ocaml-mlx/mlx). mlx-pp turns JSX into
; [@JSX]-annotated OCaml, which reason-react-ppx then lowers to React calls.
; merlin_reader makes Merlin/ocaml-lsp understand .mlx; format makes `dune fmt` work.

(dialect
 (name mlx)
 (implementation
  (extension mlx)
  (merlin_reader mlx)
  (format
   (run ocamlformat-mlx %{input-file}))
  (preprocess
   (run mlx-pp %{input-file}))))

(generate_opam_files true)

; Versions are pinned exactly: Melange, mlx and the editor tools must agree on the
; compiler (melange 7.0.1-55 = OCaml 5.5; ocaml-lsp 1.28 / merlin 5.9 = OCaml 5.5).

(package
 (name grip_gains_set_recorder)
 (synopsis "Grip Gains Set Recorder (Melange + reason-react + mlx)")
 (license MIT)
 (allow_empty)
 (depends
  (ocaml (= 5.5.2))
  (dune (= 3.25.0))
  (melange (= 7.0.1-55))
  (reason-react (= 0.17.0))
  (reason-react-ppx (= 0.17.0))
  (mlx (= 0.12))
  (ocamlmerlin-mlx (and (= 0.12) :with-dev-setup))
  (ocamlformat-mlx (and (= 0.29.0.2) :with-dev-setup))
  ; plain ocamlformat formats the .ml files; ocamlformat-mlx only handles .mlx
  (ocamlformat (and (= 0.29.0) :with-dev-setup))
  (ocaml-lsp-server (and (= 1.28.1) :with-dev-setup))))
