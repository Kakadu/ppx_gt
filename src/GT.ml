


(**************************************************************************
 *  Copyright (C) 2012-2015
 *  Dmitri Boulytchev (dboulytchev@math.spbu.ru), St.Petersburg State University
 *  Universitetskii pr., 28, St.Petersburg, 198504, RUSSIA
 *
 *  This library is free software; you can redistribute it and/or
 *  modify it under the terms of the GNU Lesser General Public
 *  License as published by the Free Software Foundation; either
 *  version 2.1 of the License, or (at your option) any later version.
 *
 *  This library is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 *  Lesser General Public License for more details.
 *
 *  You should have received a copy of the GNU Lesser General Public
 *  License along with this library; if not, write to the Free Software
 *  Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA  02110-1301 USA
 *
 *  See the GNU Lesser General Public License version 2.1 for more details
 *  (enclosed in the file COPYING).
 **************************************************************************)

type ('a, 'b) t = {gcata : 'a; plugins : 'b}
type ('a, 'b, 'c, 'd) a = {x : 'b; fx : 'a -> 'c; f : 'a -> 'b -> 'c; t : 'd}

let (~:) x = x.x
let transform t = t.gcata

let make  f x p = {x=x; fx=(fun a -> f a x); f=f; t=p}
let apply f a x = f a x

let lift f _ = f

type comparison = LT | EQ | GT

let chain_compare x f =
  match x with
  | EQ -> f ()
  | _  -> x

let compare_primitive x y =
  if x < y
  then LT
  else if x > y
       then GT
       else EQ

let poly_tag x =
  let x = Obj.magic x in
  (Obj.magic (if Obj.is_block x then Obj.field x 0 else x) : int)

let vari_tag x =
  if Obj.is_block x then Obj.tag x else Obj.magic x

let compare_poly x y =
  compare_primitive (poly_tag x) (poly_tag y)

let compare_vari x y =
  let x, y = Obj.magic x, Obj.magic y in
  match compare_primitive (Obj.is_block x) (Obj.is_block y) with
  | EQ -> compare_primitive (vari_tag x) (vari_tag y)
  | c  -> x

let string_of_string  s = s
let string_of_unit    _ = "()"
let string_of_char    c = String.make 1 c
let string_of_int32     = Int32.to_string
let string_of_int64     = Int64.to_string
let string_of_nativeint = Nativeint.to_string


type protoint = int
type int = protoint

class type show_int_env_tt    = object  end

class type virtual ['inh, 'syn] int_tt =
  object
    method t_int : 'inh -> int -> 'syn
  end

class virtual ['inh, 'syn] int_t =
  object (this)
    method virtual t_int : 'inh -> int -> 'syn
  end

class show_proto_int env =
  object (this)
    inherit [unit, string] int [@@gt_inher]
    method t_int inh x = string_of_int x
  end

class show_int_t =
  let self = Obj.magic (ref ()) in
  object (this)
    inherit [unit, string] int[@@gt_inher]
    inherit show_proto_int self
    initializer (:=) self (this :> show_int_t)
  end

let int : (('inh, 'syn) # int_tt -> 'inh -> int -> 'syn,
          < show    : int -> string >) t =
  let int_gcata t inh x = t#t_int inh x in
  {gcata = int_gcata;
   plugins =
      object
        method show    = int_gcata (new show_int_t) ()
      end
  }


type protostring = string
type string = protostring

class type show_string_env_tt    = object  end

class type virtual ['inh, 'syn] string_tt =
  object
    method t_string : 'inh -> string -> 'syn
  end

class virtual ['inh, 'syn] string_t =
  object (this)
    method virtual t_string : 'inh -> string -> 'syn
  end

class show_proto_string env =
  object (this)
    inherit [unit, string] string[@@gt_inher]
    method t_string inh x = string_of_string x
  end

class show_string_t =
  let self = Obj.magic (ref ()) in
  object (this)
    inherit [unit, string] string [@@ gt_inher]
    inherit show_proto_string self
    initializer (:=) self (this :> show_string_t)
  end

let string : (('inh, 'syn) # string_tt -> 'inh -> string -> 'syn,
          < show    : string -> string >) t =
  let string_gcata t inh x = t#t_string inh x in
  {gcata = string_gcata;
   plugins =
      object
        method show    = string_gcata (new show_string_t) ()
      end
  }


type protounit = unit
type unit = protounit

class type show_unit_env_tt    = object  end

class type virtual ['inh, 'syn] unit_tt =
  object
    method t_unit : 'inh -> unit -> 'syn
  end

class virtual ['inh, 'syn] unit_t =
  object (this)
    method virtual t_unit : 'inh -> unit -> 'syn
  end

class show_proto_unit env =
  object (this)
    inherit [unit, string] unit[@@gt_inher]
    method t_unit inh x = string_of_unit x
  end

class show_unit_t =
  let self = Obj.magic (ref ()) in
  object (this)
    inherit [unit, string] unit[@@gt_inher]
    inherit show_proto_unit self
    initializer (:=) self (this :> show_unit_t)
  end

let unit : (('inh, 'syn) # unit_tt -> 'inh -> unit -> 'syn,
          < show    : unit -> string; >) t =
  let unit_gcata t inh x = t#t_unit inh x in
  {gcata = unit_gcata;
   plugins =
      object
        method show    = unit_gcata (new show_unit_t) ()
      end
  }

type 'a plist      = 'a list
type 'a list       = 'a plist

class type html_list_env_tt = object  end
class type show_list_env_tt = object  end
class type foldl_list_env_tt = object  end
class type foldr_list_env_tt = object  end
class type eq_list_env_tt = object  end
class type compare_list_env_tt = object  end
class type map_list_env_tt = object  end

class type ['a, 'ia, 'sa, 'inh, 'syn] list_tt =
  object
    method c_Nil  : 'inh -> ('inh, 'a list, 'syn, < a : 'ia -> 'a -> 'sa >) a -> 'syn
    method c_Cons : 'inh -> ('inh, 'a list, 'syn, < a : 'ia -> 'a -> 'sa >) a ->
                                    ('ia, 'a, 'sa, < a : 'ia -> 'a -> 'sa >) a ->
                                    ('inh, 'a list, 'syn, < a : 'ia -> 'a -> 'sa >) a -> 'syn
    method t_list : ('ia -> 'a -> 'sa) -> 'inh -> 'a list -> 'syn
  end

let list : (('ia -> 'a -> 'sa) -> ('a, 'ia, 'sa, 'inh, 'syn) #list_tt -> 'inh -> 'a list -> 'syn, unit) t =
  let rec list_gcata fa trans inh subj =
    let rec self = list_gcata fa trans
    and tpo = object method a = fa end in
    match subj with
      [] -> trans#c_Nil inh (make self subj tpo)
    | p1::p2 ->
        trans#c_Cons inh (make self subj tpo) (make fa p1 tpo)
          (make self p2 tpo)
  in
  {gcata = list_gcata; plugins = ()}

class virtual ['a, 'ia, 'sa, 'inh, 'syn] list_t =
  object (this)
    method virtual c_Nil :
      'inh -> ('inh, 'a list, 'syn, < a : 'ia -> 'a -> 'sa >) a -> 'syn
    method virtual c_Cons :
      'inh -> ('inh, 'a list, 'syn, < a : 'ia -> 'a -> 'sa >) a ->
        ('ia, 'a, 'sa, < a : 'ia -> 'a -> 'sa >) a ->
        ('inh, 'a list, 'syn, < a : 'ia -> 'a -> 'sa >) a -> 'syn
    method t_list fa = transform list fa this
  end

class ['a] show_list_t =
  object
    inherit ['a, unit, string, unit, string] list[@@gt_inher]
    method c_Nil  _ _      = ""
    method c_Cons _ _ x xs = x.fx () ^ (match xs.x with [] -> "" | _ -> ", " ^ xs.fx ())
  end

let list : (('ia -> 'a -> 'sa) -> ('a, 'ia, 'sa, 'inh, 'syn) #list_tt -> 'inh -> 'a list -> 'syn,
            < show    : ('a -> string)      -> 'a list -> string;
            >) t =
  {gcata   = list.gcata;
   plugins = object
               method show    fa = transform(list) (lift fa) (new show_list_t) ()
             end
  }

type 'a poption = 'a option
type 'a option = 'a poption

class type show_option_env_tt = object  end

class type ['a, 'ia, 'sa, 'inh, 'syn] option_tt =
  object
    method c_None : 'inh -> ('inh, 'a option, 'syn, < a : 'ia -> 'a -> 'sa >) a -> 'syn
    method c_Some : 'inh -> ('inh, 'a option, 'syn, < a : 'ia -> 'a -> 'sa >) a ->
                            ('ia, 'a, 'sa, < a : 'ia -> 'a -> 'sa >) a -> 'syn
    method t_option : ('ia -> 'a -> 'sa) -> 'inh -> 'a option -> 'syn
  end

let option : (('ia -> 'a -> 'sa) -> ('a, 'ia, 'sa, 'inh, 'syn) #option_tt -> 'inh -> 'a option -> 'syn, unit) t =
  let rec option_gcata fa trans inh subj =
    let rec self = option_gcata fa trans
    and tpo = object method a = fa end in
    match subj with
      None   -> trans#c_None inh (make self subj tpo)
    | Some p -> trans#c_Some inh (make self subj tpo) (make fa p tpo)
  in
  {gcata = option_gcata; plugins = ()}

class virtual ['a, 'ia, 'sa, 'inh, 'syn] option_t =
  object (this)
    method virtual c_None :
      'inh -> ('inh, 'a option, 'syn, < a : 'ia -> 'a -> 'sa >) a -> 'syn
    method virtual c_Some :
      'inh -> ('inh, 'a option, 'syn, < a : 'ia -> 'a -> 'sa >) a ->
        ('ia, 'a, 'sa, < a : 'ia -> 'a -> 'sa >) a -> 'syn
    method t_option fa = transform option fa this
  end

class ['a] show_option_t =
  object
    inherit ['a, unit, string, unit, string] option[@@gt_inher]
    method c_None  _ _  = "None"
    method c_Some _ _ x = "Some (" ^ x.fx () ^ ")"
  end

let option : (('ia -> 'a -> 'sa) -> ('a, 'ia, 'sa, 'inh, 'syn) #option_tt -> 'inh -> 'a option -> 'syn,
              < show    : ('a -> string)      -> 'a option -> string;
              >) t =
  {gcata   = option.gcata;
   plugins = object
               method show    fa = transform(option) (lift fa) (new show_option_t) ()
             end
  }

type ('a, 'b) pair = 'a * 'b

class type show_pair_env_tt = object  end

class type ['a, 'ia, 'sa, 'b, 'ib, 'sb, 'inh, 'syn] pair_tt =
  object
    method c_Pair : 'inh -> ('inh, ('a, 'b) pair, 'syn, < a : 'ia -> 'a -> 'sa; b : 'ib -> 'b -> 'sb >) a ->
                            ('ia, 'a, 'sa, < a : 'ia -> 'a -> 'sa ; b : 'ib -> 'b -> 'sb >) a ->
                            ('ib, 'b, 'sb, < a : 'ia -> 'a -> 'sa ; b : 'ib -> 'b -> 'sb >) a -> 'syn
    method t_pair : ('ia -> 'a -> 'sa) -> ('ib -> 'b -> 'sb) -> 'inh -> ('a, 'b) pair -> 'syn
  end

let pair : (('ia -> 'a -> 'sa) -> ('ib -> 'b -> 'sb) -> ('a, 'ia, 'sa, 'b, 'ib, 'sb, 'inh, 'syn) #pair_tt -> 'inh -> ('a, 'b) pair -> 'syn, unit) t =
  let rec pair_gcata fa fb trans inh subj =
    let rec self = pair_gcata fa fb trans
    and tpo = object method a = fa method b = fb end in
    match subj with
      (a, b) -> trans#c_Pair inh (make self subj tpo) (make fa a tpo) (make fb b tpo)
  in
  {gcata = pair_gcata; plugins = ()}

class virtual ['a, 'ia, 'sa, 'b, 'ib, 'sb, 'inh, 'syn] pair_t =
  object (this)
    method virtual c_Pair :
      'inh -> ('inh, ('a, 'b) pair, 'syn, < a : 'ia -> 'a -> 'sa; b : 'ib -> 'b -> 'sb >) a ->
        ('ia, 'a, 'sa, < a : 'ia -> 'a -> 'sa; b : 'ib -> 'b -> 'sb >) a ->
        ('ib, 'b, 'sb, < a : 'ia -> 'a -> 'sa; b : 'ib -> 'b -> 'sb >) a -> 'syn
    method t_pair fa fb = transform pair fa fb this
  end

class ['a, 'b] show_pair_t =
  object
    inherit ['a, unit, string, 'b, unit, string, unit, string] pair[@@gt_inher]
    method c_Pair _ _ x y = "(" ^ x.fx () ^ ", " ^ y.fx () ^ ")"
  end


let pair : (('ia -> 'a -> 'sa) -> ('ib -> 'b -> 'sb) -> ('a, 'ia, 'sa, 'b, 'ib, 'sb, 'inh, 'syn) #pair_tt -> 'inh -> ('a, 'b) pair -> 'syn,
              < show    : ('a -> string) -> ('b -> string) -> ('a, 'b) pair -> string;
              >) t =
  {gcata   = pair.gcata;
   plugins = object
               method show    fa fb = transform(pair) (lift fa) (lift fb) (new show_pair_t) ()
             end
  }

let show    t = t.plugins#show
