#!/usr/bin/env ocaml
#directory "pkg"
#use "topkg.ml"

let () =
  Pkg.describe "ppx_deriving_morphism" ~builder:`OCamlbuild [
    Pkg.lib "pkg/META";
    (* Pkg.lib ~exts:Exts.library "src/ppx_deriving_morphism"; *)
    (* Pkg.lib ~exts:Exts.library "src/ppx_deriving_folder"; *)
    (* Pkg.lib ~exts:Exts.library "src/ppx_deriving_mapper"; *)
    (* Pkg.lib ~exts:Exts.library "src/GT"; *)
    Pkg.lib ~exts:Exts.library "src/ppx_deriving_gt";
    Pkg.doc "README.md";
    Pkg.doc "LICENSE";
    Pkg.doc "CHANGELOG.md"; ]
