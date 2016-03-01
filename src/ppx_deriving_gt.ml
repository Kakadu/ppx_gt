(*
 * Copyright (c) 2014, TU Berlin
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions are met:
 *   * Redistributions of source code must retain the above copyright
 *     notice, this list of conditions and the following disclaimer.
 *   * Redistributions in binary form must reproduce the above copyright
 *     notice, this list of conditions and the following disclaimer in the
 *     documentation and/or other materials provided with the distribution.
 *   * Neither the name of the TU Berlin nor the
 *     names of its contributors may be used to endorse or promote products
 *     derived from this software without specific prior written permission.

 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
 * ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
 * WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
 * DISCLAIMED. IN NO EVENT SHALL TU Berlin BE LIABLE FOR ANY
 * DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
 * (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
 * LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND
 * ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
 * (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
 * SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 *
 *)

open Printf
open Longident
open Location
open Asttypes
open Parsetree
open Ast_helper
open Ast_convenience
open Ppx_deriving

let deriver = "gt"
let raise_errorf = Ppx_deriving.raise_errorf

type supported_derivers = { gt_show: bool; gt_eq: bool }

let filter_map f xs =
  List.fold_right (fun x acc -> match f x with Some v -> v::acc | None -> acc) xs []

let parse_options options =
  List.fold_left (fun acc (name,expr) ->
    match name with
    | "show" -> {acc with gt_show = true}
    | _ -> raise_errorf ~loc:expr.pexp_loc "%s does not support option %s" deriver name)
    { gt_show=false; gt_eq=false }
    options

let attr_nobuiltin attrs =
  Ppx_deriving.(attrs |> attr ~deriver "nobuiltin" |> Arg.get_flag ~deriver)

let attr_printer attrs =
  Ppx_deriving.(attrs |> attr ~deriver "printer" |> Arg.(get_attr ~deriver expr))

let attr_polyprinter attrs =
  Ppx_deriving.(attrs |> attr ~deriver "polyprinter" |> Arg.(get_attr ~deriver expr))

let attr_opaque attrs =
  Ppx_deriving.(attrs |> attr ~deriver "opaque" |> Arg.get_flag ~deriver)

let argn = Printf.sprintf "a%d"
(*
let pp_type_of_decl ~options ~path type_decl =
  let opts = parse_options options in
  let typ = Ppx_deriving.core_type_of_type_decl type_decl in
  Ppx_deriving.poly_arrow_of_type_decl
    (fun var -> [%type: Format.formatter -> [%t var] -> Ppx_deriving_runtime.unit])
    type_decl
    [%type: Format.formatter -> [%t typ] -> Ppx_deriving_runtime.unit]

let show_type_of_decl ~options ~path type_decl =
  let opts  = parse_options options in
  let typ = Ppx_deriving.core_type_of_type_decl type_decl in
  Ppx_deriving.poly_arrow_of_type_decl
    (fun var -> [%type: Format.formatter -> [%t var] -> Ppx_deriving_runtime.unit])
    type_decl
    [%type: [%t typ] -> Ppx_deriving_runtime.string]

let sig_of_type ~options ~path type_decl =
  let opts = parse_options options in
  [Sig.value (Val.mk (mknoloc (Ppx_deriving.mangle_type_decl (`Prefix "pp") type_decl))
              (pp_type_of_decl ~options ~path type_decl));
   Sig.value (Val.mk (mknoloc (Ppx_deriving.mangle_type_decl (`Prefix "show") type_decl))
              (show_type_of_decl ~options ~path type_decl))]

let rec expr_of_typ quoter typ =
  let expr_of_typ = expr_of_typ quoter in
  match attr_printer typ.ptyp_attributes with
  | Some printer ->
    let printer =
      [%expr (let fprintf = Format.fprintf in [%e printer]) [@ocaml.warning "-26"]]
    in
    [%expr [%e Ppx_deriving.quote quoter printer] fmt]
  | None ->
  if attr_opaque typ.ptyp_attributes then
    [%expr fun _ -> Format.pp_print_string fmt "<opaque>"]
  else
    let format x = [%expr Format.fprintf fmt [%e str x]] in
    let seq start finish fold typ =
      [%expr fun x ->
        Format.fprintf fmt [%e str start];
        ignore ([%e fold] (fun sep x ->
          if sep then Format.fprintf fmt ";@ ";
          [%e expr_of_typ typ] x; true) false x);
        Format.fprintf fmt [%e str finish];]
    in
    match typ with
    | [%type: _] -> [%expr fun _ -> Format.pp_print_string fmt "_"]
    | { ptyp_desc = Ptyp_arrow _ } ->
      [%expr fun _ -> Format.pp_print_string fmt "<fun>"]
    | { ptyp_desc = Ptyp_constr _ } ->
      let builtin = not (attr_nobuiltin typ.ptyp_attributes) in
      begin match builtin, typ with
      | true, [%type: unit]        -> [%expr fun () -> Format.pp_print_string fmt "()"]
      | true, [%type: int]         -> format "%d"
      | true, [%type: int32]
      | true, [%type: Int32.t]     -> format "%ldl"
      | true, [%type: int64]
      | true, [%type: Int64.t]     -> format "%LdL"
      | true, [%type: nativeint]
      | true, [%type: Nativeint.t] -> format "%ndn"
      | true, [%type: float]       -> format "%F"
      | true, [%type: bool]        -> format "%B"
      | true, [%type: char]        -> format "%C"
      | true, [%type: string]
      | true, [%type: String.t]    -> format "%S"
      | true, [%type: bytes]
      | true, [%type: Bytes.t] ->
        [%expr fun x -> Format.fprintf fmt "%S" (Bytes.to_string x)]
      | true, [%type: [%t? typ] ref] ->
        [%expr fun x ->
          Format.pp_print_string fmt "ref (";
          [%e expr_of_typ typ] !x;
          Format.pp_print_string fmt ")"]
      | true, [%type: [%t? typ] list]  -> seq "[@[<hov>"   "@]]" [%expr List.fold_left]  typ
      | true, [%type: [%t? typ] array] -> seq "[|@[<hov>" "@]|]" [%expr Array.fold_left] typ
      | true, [%type: [%t? typ] option] ->
        [%expr
          function
          | None -> Format.pp_print_string fmt "None"
          | Some x ->
            Format.pp_print_string fmt "(Some ";
            [%e expr_of_typ typ] x;
            Format.pp_print_string fmt ")"]
      | true, ([%type: [%t? typ] lazy_t] | [%type: [%t? typ] Lazy.t]) ->
        [%expr fun x ->
          if Lazy.is_val x then [%e expr_of_typ typ] (Lazy.force x)
          else Format.pp_print_string fmt "<not evaluated>"]
      | _, { ptyp_desc = Ptyp_constr ({ txt = lid }, args) } ->
        let args_pp = List.map (fun typ -> [%expr fun fmt -> [%e expr_of_typ typ]]) args in
        let printer =
          match attr_polyprinter typ.ptyp_attributes with
          | Some printer ->
            [%expr (let fprintf = Format.fprintf in [%e printer]) [@ocaml.warning "-26"]]
          | None ->
            Exp.ident (mknoloc (Ppx_deriving.mangle_lid (`Prefix "pp") lid))
        in
        app (Ppx_deriving.quote quoter printer)
            (args_pp @ [[%expr fmt]])
      | _ -> assert false
      end
    | { ptyp_desc = Ptyp_tuple typs } ->
      let args = List.mapi (fun i typ -> app (expr_of_typ typ) [evar (argn i)]) typs in
      [%expr
        fun [%p ptuple (List.mapi (fun i _ -> pvar (argn i)) typs)] ->
        Format.fprintf fmt "(@[<hov>";
        [%e args |> Ppx_deriving.(fold_exprs
                (seq_reduce ~sep:[%expr Format.fprintf fmt ",@ "]))];
        Format.fprintf fmt "@])"]
    | { ptyp_desc = Ptyp_variant (fields, _, _); ptyp_loc } ->
      let cases =
        fields |> List.map (fun field ->
          match field with
          | Rtag (label, _, true (*empty*), []) ->
            Exp.case (Pat.variant label None)
                     [%expr Format.pp_print_string fmt [%e str ("`" ^ label)]]
          | Rtag (label, _, false, [typ]) ->
            Exp.case (Pat.variant label (Some [%pat? x]))
                     [%expr Format.fprintf fmt [%e str ("`" ^ label ^ " (@[<hov>")];
                            [%e expr_of_typ typ] x;
                            Format.fprintf fmt "@])"]
          | Rinherit ({ ptyp_desc = Ptyp_constr (tname, _) } as typ) ->
            Exp.case [%pat? [%p Pat.type_ tname] as x]
                     [%expr [%e expr_of_typ typ] x]
          | _ ->
            raise_errorf ~loc:ptyp_loc "%s cannot be derived for %s"
                         deriver (Ppx_deriving.string_of_core_type typ))
      in
      Exp.function_ cases
    | { ptyp_desc = Ptyp_var name } -> [%expr [%e evar ("poly_"^name)] fmt]
    | { ptyp_desc = Ptyp_alias (typ, _) } -> expr_of_typ typ
    | { ptyp_loc } ->
      raise_errorf ~loc:ptyp_loc "%s cannot be derived for %s"
                   deriver (Ppx_deriving.string_of_core_type typ)
      *)
module Exp = struct
  include Exp

  let make_pair a b = tuple [a;b]
  let make_list xs =
    List.fold_right (fun e acc -> construct (lid "::") (Some (make_pair e acc)) ) xs
                                            (Exp.construct (lid "[]") None)

end

let str_of_type ~options ~path ({ ptype_loc = loc } as type_decl) =
  let { gt_show; _ } = parse_options options in
  let _quoter = Ppx_deriving.create_quoter () in
  let path = Ppx_deriving.path_of_type_decl ~path type_decl in

  let typename = type_decl.ptype_name.txt in
  match type_decl.ptype_kind with
  | Ptype_variant constrs ->
      let _fields =
        constrs |> List.map (fun { pcd_name = { txt = name' }; pcd_args } ->
          let constr_name = Ppx_deriving.expand_path ~path name' in
          Ast_helper.Cf.method_ (Location.mknoloc constr_name) Public
            (Cfk_concrete (Fresh,
                           [%expr 1]))
        )

      in
      let default_params =
        let ps = type_decl.ptype_params |> List.map (fun (t,_) ->
          match t.ptyp_desc with
          | Ptyp_var n -> Typ.([var n; var @@ "i"^n; var @@ "s"^n])
          | _ -> raise_errorf "default_params: can't construct"
        )
        in
        let ps = List.concat ps in
        let ps = ps @ [ [%type: 'inh]; [%type: 'syn] ] in
        List.map (fun x -> (x,Invariant) ) ps
      in
      let using_type =
        (* generation type specification by type declaration *)
        Typ.constr (lid typename) (List.map fst @@ type_decl.ptype_params)
      in
      let arr_of_param t =
        (* does from 'a the 'ia -> 'a -> 'sa *)
        let open Typ in
        match t.ptyp_desc with
        | Ptyp_var n ->
            (n, [], [%type: [%t var@@ "i"^n] -> [%t var n] -> [%t var @@ "s"^n]] )
        | _ ->
            raise_errorf "arr_of_param: not all type params are supported" deriver
      in
      let params_obj =
        let f (t,_) = arr_of_param t in
        Typ.object_ (List.map f type_decl.ptype_params) Closed
      in
      let subclass_obj =
        (* makes ('a,'ia,'sa,...,'inh,'syn)#typename_tt  *)
        Typ.class_ (lid @@ typename^"_tt") (List.map fst default_params)
      in
      let (tt_methods, t_methods) =
        let xs = List.map (fun { pcd_name = { txt = name' }; pcd_args } ->
          (* for every type constructor *)
          let constr_name = "c_" ^ name' in
          let args2 = pcd_args |> List.map (fun ({ ptyp_desc; _ } as typ) ->
            (* let (_:int) = ptyp_desc in *)
            match ptyp_desc with
            | Ptyp_constr ({txt=Ldot (Lident "GT", "int"); _},[]) ->
                [%type: GT.int]
            | Ptyp_constr _ ->
                [%type: ([%t Typ.var @@ "inh"],
                         [%t typ],
                         [%t Typ.var @@ "syn"],
                         [%t params_obj]) GT.a ]
            | Ptyp_var a  ->
                [%type: ([%t Typ.var @@ "i"^a],
                         [%t typ ],
                         [%t Typ.var @@ "s"^a],
                         [%t params_obj]) GT.a ]
            (* | Ptyp_constr _ -> typ *)
            | _ -> raise_errorf "Some cases are not supported when we look at constructor's params"
          )
          in
          let args2 = [%type: ('inh,[%t using_type],'syn, [%t params_obj]) GT.a ]
                      (* [Typ.constr (lid "GT.a") [[%typ]] *)
                      :: args2
          in
          let args2 = [Typ.var "inh"] @ args2 in
          let ts = List.fold_right (Typ.arrow Parr_simple) args2 (Typ.var "syn") in

          (Ctf.method_ constr_name Public Concrete ts,
           Cf.method_  (mknoloc constr_name) Public (Cfk_virtual ts) )
        ) constrs
        in
        let (tts, ts) = List.split xs in
        let tts = tts @
                  [
                    let ts = List.map (fun (t,_) -> arr_of_param t) type_decl.ptype_params in
                    let init =
                      [%type: 'inh -> [%t using_type] -> 'syn ]
                    in
                    Ctf.method_ ("t_" ^ typename)  Public Concrete
                      (List.fold_right (fun (_,_,x) acc -> Typ.arrow Parr_simple x acc) ts init)
                  ]
        in

        let main_mapper_body =
          (* TODO: generate based on type parameters *)
          [%expr fun fa -> GT.transform [%e Exp.ident @@ lid typename] fa this]
        in
        let ts = ts @ [ Cf.method_ (mknoloc @@ "t_"^typename) Public
                          (Cfk_concrete (Fresh, main_mapper_body))
                      ]
        in
        (tts, ts)
      in
      let any_typ = { ptyp_desc=Ptyp_any; ptyp_loc=Location.none; ptyp_attributes=[] } in

      let gt_repr_typ_wrap arg =
        let tail = [%type: 'inh -> [%t using_type] -> 'syn ] in
        [%type:
                (('ia -> 'a -> 'sa) ->
                 [%t subclass_obj ] -> [%t tail], [%t arg]) GT.t ]
      in
      let gt_repr_typ = gt_repr_typ_wrap [%type: unit] in
      let gt_repr_body =
        let typename_gcata = typename^"_gcata" in
        let tpo_meths =
          let f ({ptyp_desc; _},_) =
            match ptyp_desc with
            | Ptyp_var v -> Cf.method_ (mknoloc v) Public (Cfk_concrete (Fresh, Exp.ident @@ lid ("f"^v)))
            | _ -> raise_errorf "Some cases are not supported when creating tpo methods"
          in
          List.map f type_decl.ptype_params
        in
        let tpo = Exp.object_ (Cstr.mk (Pat.any ()) tpo_meths ) in
        let match_body =
          Exp.match_ (Exp.ident @@ lid "subj") @@
          ListLabels.map constrs ~f:(fun { pcd_name = { txt = name' }; pcd_args } ->
            let argnames = List.mapi (fun n _ -> sprintf "p%d" n) pcd_args in
            let args_tuple =
              match argnames with
              | [] -> None
              | [single_arg] -> Some (Pat.var @@ mknoloc single_arg)
              | _ -> Some (Pat.tuple @@ List.map (fun s-> Pat.var @@ mknoloc s) argnames)
            in

            let app_args = List.map2 (fun argname arg ->
              match arg.ptyp_desc with
              | Ptyp_var v -> [%expr GT.make [%e Exp.ident @@ lid @@ "f"^v] [%e Exp.ident @@ lid argname] tpo]
              | Ptyp_constr ({txt=Ldot (Lident "GT", "int"); _},[]) ->
                  [%expr [%e Exp.ident @@ lid argname]]
              | Ptyp_constr _ ->
                 [%expr GT.make [%e Exp.ident @@ lid "self"] [%e Exp.ident @@ lid argname] tpo]
              | _ -> raise_errorf "Some cases are not supported when gnerating application in gcata"
            ) argnames pcd_args
            in

            Exp.case (Pat.construct (lid name') args_tuple) @@
            Exp.(apply (send (ident @@ lid "trans") ("c_"^name') )
                 @@ List.map (fun x -> (Papp_simple,x))
                   ([ [%expr inh]; [%expr (GT.make self subj tpo)] ] @ app_args)
                )
          )
        in
        [%expr
          let rec [%p (Pat.var @@ mknoloc typename_gcata) ] = fun fa (* TODO: generate patterns there *)
            trans inh subj ->
            let rec self = [%e Exp.ident @@ lid typename_gcata] fa trans
            and tpo = [%e tpo ] in
            [%e match_body]
          in
          { GT.gcata = [%e Exp.ident @@ lid typename_gcata]; GT.plugins = () }

        ]
      in

      let ans =
        [ Str.class_type [Ci.mk ~virt:Virtual ~params:default_params
                            (Location.mknoloc @@ type_decl.ptype_name.txt ^ "_tt") @@
                          Cty.signature (Csig.mk any_typ tt_methods)]
        ; Str.value Nonrecursive [Vb.mk (Pat.(constraint_ (var @@ mknoloc typename) gt_repr_typ)) gt_repr_body]
        ; Str.class_ [Ci.mk ~virt:Virtual ~params:default_params (mknoloc @@ typename^"_t") @@
                      Cl.structure (Cstr.mk (Pat.var @@ mknoloc "this") t_methods)
                     ]
        ]
      in
      let show_decls () =
        let typename_t = typename ^ "_t" in
        let show_typename_t = "show_" ^ typename_t in
        let inherit_field =
          Cf.inherit_ Fresh (Cl.constr (lid typename_t) Typ.[var "a"; constr (lid "unit") []; constr (lid "string") []; constr (lid "unit") []; constr (lid "string") [] ]) None
        in
        let show_proto_meths =
          let f { pcd_name = { txt = name' }; pcd_args } =
            let args = List.mapi (fun n _ -> sprintf "p%d" n) pcd_args in

            let body =
              let expr_of_arg reprname = function
                | Ptyp_var _alpha ->
                   [%expr [%e Exp.(field (ident @@ lid reprname) (lid "GT.fx")) ]
                          () ]
                | Ptyp_constr ({txt=Ldot (Lident "GT", "int");_}, []) ->
                   [%expr GT.transform GT.int (new GT.show_int_t) () [%e Exp.ident @@ lid reprname] ]
                | Ptyp_constr ({txt=Ldot (Lident "GT", "string");_}, [])
                | Ptyp_constr ({txt=Lident "string";_}, []) ->
                   [%expr GT.transform GT.string (new GT.show_string_t) () [%e Exp.ident @@ lid reprname] ]
                | _ ->
                    [%expr [%e Exp.(field (ident @@ lid reprname) (lid "GT.fx")) ] () ]
                    (* Exp.constant (Const_string ("<not_implemented>", None)) *)
              in
              match List.combine args pcd_args with
              | [] -> Exp.constant (Const_string (name', None))
              | [(name,argt)] -> [%expr
                            [%e Exp.constant (Const_string (name'^" ", None)) ] ^
                            [%e expr_of_arg name argt.ptyp_desc ]
                                 ]
              | args ->
                 let xs = List.map (fun (name,arg) -> expr_of_arg name arg.ptyp_desc) args in
                 (* [%expr 1] *)
                 [%expr
                     [%e Exp.constant (Const_string (name'^" (", None)) ] ^
                     (String.concat ", " [%e Exp.make_list xs ] ^ ")")
                 ]
                 (* List.fold_right (fun e acc -> Exp.construct (lid "::") Some (pe)None) xs *)
                 (*                 (Exp.construct (lid "[]") None) *)
                 (* assert false *)
            in
            let e = List.fold_right (fun name acc -> Exp.fun_ Parr_simple None (Pat.var @@ mknoloc name) acc) args body in
            let e = [%expr fun inh subj -> [%e e] ] in

            Cf.method_ (mknoloc @@ "c_"^name') Public (Cfk_concrete (Fresh, e))
          in
          inherit_field :: List.map f constrs
        in

        let gt_repr_typ_show =
          gt_repr_typ_wrap [%type: < show: ('a -> string) -> [%t Typ.constr (lid typename) [Typ.var "a"] ] -> string > ]
        in
        let proto_class_name = "show_proto_" ^ typename in
        [ Str.class_type [Ci.mk ~virt:Concrete ~params:type_decl.ptype_params
                            (mknoloc @@ sprintf "show_%s_env_tt" typename) @@
                          Cty.signature (Csig.mk any_typ [])
                         ]
        ; Str.class_ [Ci.mk ~virt:Concrete ~params:[Typ.var "a",Invariant] (mknoloc proto_class_name)
                        (Cl.fun_ Parr_simple None (Pat.var @@ mknoloc "env") @@
                         Cl.structure (Cstr.mk (Pat.var @@ mknoloc "this") show_proto_meths)
                        )
                     ]
        ; Str.class_ [Ci.mk ~virt:Concrete ~params:[Typ.var "a",Invariant] (mknoloc show_typename_t)
                        (Cl.let_ Nonrecursive [Vb.mk (Pat.var @@ mknoloc "self") [%expr Obj.magic (ref ())] ] @@
                         Cl.structure (Cstr.mk (Pat.var @@ mknoloc "this")
                                         [ inherit_field
                                         ; Cf.inherit_ Fresh (Cl.apply (Cl.constr (lid proto_class_name) [Typ.var "a"]) [(Papp_simple,[%expr self])]) None
                                         ; Cf.initializer_ [%expr self := (this :> [%t Typ.constr (lid show_typename_t) [Typ.var "a"] ]) ]
                                         ])
                        )
                     ]
          (* gcata for show *)
        ; Str.value Nonrecursive
            [Vb.mk (Pat.(constraint_ (var @@ mknoloc typename) gt_repr_typ_show))
               [%expr
                   { GT.gcata = [%e Exp.(field (ident @@ lid typename) (lid "GT.gcata")) ];
                     GT.plugins = object
                        method show a = GT.transform [%e Exp.ident @@ lid typename]
                                                     (GT.lift a)
                                                     [%e Exp.new_ (lid show_typename_t)] ()
                                  end }
               ] ]
        ]
      in

      let ans = if not gt_show then ans else ans @ (show_decls ()) in
      ans

  | _ -> raise_errorf ~loc "%s: some error" deriver
  (*
  let prettyprinter =
    match type_decl.ptype_kind, type_decl.ptype_manifest with
    | Ptype_abstract, Some manifest ->
      [%expr fun fmt -> [%e expr_of_typ quoter manifest]]
    | Ptype_variant constrs, _ ->
      let cases =
        constrs |> List.map (fun { pcd_name = { txt = name' }; pcd_args } ->
          let constr_name = Ppx_deriving.expand_path ~path name' in
          let args =
            List.mapi (fun i typ -> app (expr_of_typ quoter typ) [evar (argn i)]) pcd_args
          in
          let result =
            match args with
            | []   -> [%expr Format.pp_print_string fmt [%e str constr_name]]
            | [arg] ->
              [%expr
                Format.fprintf fmt [%e str ("(@[<hov2>" ^  constr_name ^ "@ ")];
                [%e arg];
                Format.fprintf fmt "@])"]
            | args ->
              [%expr Format.fprintf fmt [%e str ("@[<hov2>" ^  constr_name ^ " (@,")];
              [%e args |> Ppx_deriving.(fold_exprs
                    (seq_reduce ~sep:[%expr Format.fprintf fmt ",@ "]))];
              Format.fprintf fmt "@])"]
          in
          Exp.case (pconstr name' (List.mapi (fun i _ -> pvar (argn i)) pcd_args)) result)
      in
      [%expr fun fmt -> [%e Exp.function_ cases]]
    | Ptype_record labels, _ ->
      let fields =
        labels |> List.mapi (fun i { pld_name = { txt = name }; pld_type; pld_attributes } ->
          let field_name = if i = 0 then Ppx_deriving.expand_path ~path name else name in
          let pld_type = {pld_type with ptyp_attributes=pld_attributes@pld_type.ptyp_attributes} in
          [%expr Format.pp_print_string fmt [%e str (field_name ^ " = ")];
            [%e expr_of_typ quoter pld_type] [%e Exp.field (evar "x") (mknoloc (Lident name))]])
      in
      [%expr fun fmt x ->
        Format.fprintf fmt "{ @[<hov>";
        [%e fields |> Ppx_deriving.(fold_exprs
              (seq_reduce ~sep:[%expr Format.fprintf fmt ";@ "]))];
        Format.fprintf fmt "@] }"]
    | Ptype_abstract, None ->
      raise_errorf ~loc "%s cannot be derived for fully abstract types" deriver
    | Ptype_open, _        ->
      raise_errorf ~loc "%s cannot be derived for open types" deriver
  in
  let pp_poly_apply = Ppx_deriving.poly_apply_of_type_decl type_decl (evar
                        (Ppx_deriving.mangle_type_decl (`Prefix "pp") type_decl)) in
  let stringprinter = [%expr fun x -> Format.asprintf "%a" [%e pp_poly_apply] x] in
  let polymorphize  = Ppx_deriving.poly_fun_of_type_decl type_decl in
  let pp_type =
    Ppx_deriving.strong_type_of_type @@ pp_type_of_decl ~options ~path type_decl in
  let show_type =
    Ppx_deriving.strong_type_of_type @@
      show_type_of_decl ~options ~path type_decl in
  let pp_var =
    pvar (Ppx_deriving.mangle_type_decl (`Prefix "pp") type_decl) in
  let show_var =
    pvar (Ppx_deriving.mangle_type_decl (`Prefix "show") type_decl) in
  [Vb.mk (Pat.constraint_ pp_var pp_type)
         (Ppx_deriving.sanitize ~quoter (polymorphize prettyprinter));
   Vb.mk (Pat.constraint_ show_var show_type) (polymorphize stringprinter);]
  *)

let register () =
  Ppx_deriving.(register (create deriver
    (* ~core_type: (Ppx_deriving.with_quoter (fun quoter typ -> *)
    (*   [%expr fun x -> Format.asprintf "%a" (fun fmt -> [%e expr_of_typ quoter typ]) x])) *)
    ~type_decl_str: (fun ~options ~path type_decls ->
      List.concat (List.map (str_of_type ~options ~path) type_decls))
    (* ~type_decl_sig: (fun ~options ~path type_decls -> *)
    (*   List.concat (List.map (sig_of_type ~options ~path) type_decls)) *)
    ()
  ))

let () = register ()
