open Ocamlbuild_plugin

let () = dispatch (
  function
  | After_rules ->
    let ppx_loc = (Findlib.query "ppx_deriving").Findlib.location in
    let std_deriver deriver =  ppx_loc ^ "/" ^ deriver in

    (* flag ["ocaml";"link";"byte";"use_morphism"] & *)
    (* A (ppx_loc ^ "/ppx_deriving_runtime.cma") ; *)

    (* flag ["ocaml";"link";"native";"use_morphism"] & *)
    (* A (ppx_loc ^ "/ppx_deriving_runtime.cmxa") ; *)

    (* flag ["ocaml"; "compile"; "use_morphism"] & *)
    (* S[(\* A"-ppx"; A"ocamlfind ppx_import/ppx_import"; *\) *)
    (*   A"-I"; A ppx_loc ; *)
    (*   A"-ppx"; A("ocamlfind ppx_deriving/ppx_deriving "^ *)
    (*              "src/ppx_deriving_folder.cma "^ *)
    (*              "src/ppx_deriving_mapper.cma "^ *)
    (*              (std_deriver "ppx_deriving_show.cma")) ; *)
    (*  ]; *)

    (* flag ["ocaml";"link";"byte";"use_gt"] & *)
    (* A (ppx_loc ^ "/ppx_deriving_gt.cma") ; *)

    (* flag ["ocaml";"link";"native";"use_gt"] & *)
    (* A (ppx_loc ^ "/ppx_deriving_gt.cmxa") ; *)

    rule "Generate a cmxs from a cmxa"
      ~dep:"%.cmxa"
      ~prod:"%.cmxs"
      ~insert:`top
      (fun env _ ->
         Cmd (S [ !Options.ocamlopt
                ; A "-shared"
                ; A "-linkall"
                ; A "-I"; A (Pathname.dirname (env "%"))
                ; A (env "%.cmxa")
                ; A "-o"
                ; A (env "%.cmxs")
                ]));

    flag ["ocaml"; "link"; "use_gt"] & S [  A"-I"; A"src";    A"GT.cmo" ];
    flag ["ocaml"; "compile"; "use_gt"] &
    S[(* A"-ppx"; A"ocamlfind ppx_import/ppx_import"; *)
      A"-I"; A ppx_loc ;
      A"-dsource";
      A"-ppx"; A("ocamlfind ppx_deriving/ppx_deriving "^
                 "src/ppx_deriving_gt.cma "(* ^ *)
                 (* (std_deriver "ppx_deriving_show.cma") *)) ;
     ];


  | _ -> ())
