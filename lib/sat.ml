(* MIT License

  Copyright (c) 2023 DaimyoHub

  Permission is hereby granted, free of charge, to any person obtaining a copy
  of this software and associated documentation files (the "Software"), to deal
  in the Software without restriction, including without limitation the rights
  to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
  copies of the Software, and to permit persons to whom the Software is
  furnished to do so, subject to the following conditions:

  The above copyright notice and this permission notice shall be included in all
  copies or substantial portions of the Software.

  THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
  IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
  FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
  AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
  LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
  OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
  SOFTWARE. *)


type tri_t = T | F | U

type lit_t = V of int | Nv of int | Tlit | Flit

type clause_t = lit_t list

type cnf_t = clause_t list

type valuation_t = (int * tri_t) list


module Clause =
struct
  let rec search_and_remove_positive id c =
    match c with
    | [] | [Tlit] | [Flit] -> c
    | V(x) :: s when x = id -> search_and_remove_positive id s
    | lit :: s -> lit :: search_and_remove_positive id s


  let rec search_and_remove_negative id c =
    match c with
    | [] | [Tlit] | [Flit] -> c
    | Nv(x) :: s when x = id -> search_and_remove_negative id s
    | lit :: s -> lit :: search_and_remove_negative id s


  let transform t id c =
    if t = F then
      if c = [V(id)] then [Flit]
      else
        let res_c = if List.mem (Nv id) c then [Tlit] else c in
        search_and_remove_positive id res_c
    else
      if c = [Nv(id)] then [Flit]
      else
        let res_c = if List.mem (V id) c then [Tlit] else c in
        search_and_remove_negative id res_c


  let substitute_and_transform t id c =
    match t with
    | U -> failwith "You can not substitute a literal with U in a clause."
    | _ -> transform t id c


  let evaluate c =
    let tri_or ea b =
      let eb = (function Tlit -> T | Flit -> F | _ -> U) b in
      match ea, eb with
      | T, _ | _, T -> T
      | U, _ | _, U -> U
      | F, F -> F
    in
    List.fold_left tri_or F c


  let find_max_id c = 
    let rec _find_max_id c m =
      match c with
      | [] -> m
      | V id :: s -> if id > m then _find_max_id s id else _find_max_id s m
      | Nv id :: s -> if id > m then _find_max_id s id else _find_max_id s m
      | _ :: s -> _find_max_id s m
    in
    _find_max_id c (-1)
end


module Cnf =
struct
  let evaluate cnf =
    let tri_and ea b =
      let eb = Clause.evaluate b in
      match ea, eb with
      | F, _ | _, F -> F
      | U, _ | _, U -> U
      | T, T -> T
    in
    List.fold_left tri_and T cnf


  let rec transform t id cnf =
    match cnf with
    | [] -> []
    | c :: s -> Clause.substitute_and_transform t id c :: transform t id s

  
  let find_max_literal_id cnf =
    let rec find_max_id cnf m =
      match cnf with
      | [] -> m
      | c :: s -> let mc = Clause.find_max_id c in
          if mc > m then find_max_id s mc else find_max_id s m
    in
    find_max_id cnf 0
  

  let search_satisfying_valuation cnf =
    let rec find_valuation cnf partial_sol id =
      match evaluate cnf with
      | T -> Some partial_sol
      | F -> None
      | U -> begin
          let res_t =
            find_valuation (transform T id cnf) ((id, T)::partial_sol) (id + 1)
          and res_f =
            find_valuation (transform F id cnf) ((id, F)::partial_sol) (id + 1)
          in 
          if res_t <> None then res_t else res_f
        end
    in
    find_valuation cnf [] 0
end
