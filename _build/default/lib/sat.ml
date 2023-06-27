(** The tri_t type allows us to partially evaluate a formula. *)
type tri_t = T | F | U

(** A literal is theorically represented by either a simple propositional
    variable 'x' or a formula of the form 'not(x)'. 

    In practice, we also add two literal types of the form Tlit and Flit, which
    respectively represent the truth value and the falsity value. It is used
    to represent a clause which is either evaluated to true or to false. *)
type lit_t = V of int | Nv of int | Tlit | Flit

type clause_t = lit_t list

type cnf_t = clause_t list

type valuation_t = (int * tri_t) list


module Clause =
struct
  (*
   * Finds and removes all occurences of V(id) in the given clause c.
   * 
   * Output :
   *   A transformed clause : it contains no more V(id).
   *
   * Inputs :
   *   id - id of the removed literal
   *   c - transformed clause
   *)
  let rec find_and_remove_positive id c =
    match c with
    | [] | [Tlit] | [Flit] -> c
    | V(x) :: s when x = id -> find_and_remove_positive id s
    | lit :: s -> lit :: find_and_remove_positive id s


  (*
   * Same as find_and_remove_positive but it removes occurences of Nv(id)
   * instead of V(id).
   *)
  let rec find_and_remove_negative id c =
    match c with
    | [] | [Tlit] | [Flit] -> c
    | Nv(x) :: s when x = id -> find_and_remove_negative id s
    | lit :: s -> lit :: find_and_remove_negative id s


  (*
   * Transforms the given clause. See substitute_and_transform for more
   * information.
   *)
  let transform t id c =
    if t = F then
      if c = [V(id)] then [Flit]
      else
        let res_c = if List.mem (Nv id) c then [Tlit] else c in
        find_and_remove_positive id res_c
    else
      if c = [Nv(id)] then [Flit]
      else
        let res_c = if List.mem (V id) c then [Tlit] else c in
        find_and_remove_negative id res_c


  (*
   * Substitutes a given literal with a given value in the clause, then
   * simplifies it thanks to its evaluation.
   *
   * Here is an example :
   *   # substitute_and_transform F 1 [V 1; Nv 2; V 3]
   *   - : lit_t list = [Nv 2; V 3]
   * 
   *   # substitute_and_transform F 2 [V 1; Nv 2; V 3]
   *   - : lit_t list = [Tlit]
   * 
   * Output :
   *   A transformed clause.
   *
   * Inputs :
   *   t - substitution value applied to the clause
   *   id - substituted literal
   *   c - transformed clause
   *)
  let substitute_and_transform t id c =
    match t with
    | U -> failwith "You can not substitute a literal with U in a clause."
    | _ -> transform t id c


  (* Evaluates the given clause to a trilean value. *)
  let evaluate c =
    let evaluate_literal lit =
      match lit with
      | Tlit -> T
      | Flit -> F
      | _ -> U
    in
    let tri_or ea b =
      let eb = evaluate_literal b in
      match ea, eb with
      | T, _ | _, T -> T
      | U, _ | _, U -> U
      | F, F -> F
    in
    List.fold_left tri_or F c


  (* Finds the maximum literal id in the given clause. *)
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
  (* Evaluates the given CNF formula to a trilean value. *)
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

  
  let max_literal_id cnf =
    let rec find_max_id cnf m =
      match cnf with
      | [] -> m
      | c :: s -> let mc = Clause.find_max_id c in
          if mc > m then find_max_id s mc else find_max_id s m
    in
    find_max_id cnf 0
  

  let find_satisfying_valuation cnf =
    let rec find_valuation cnf partial_sol id =
      match evaluate cnf with
      | T -> Some partial_sol
      | F -> None
      | U -> begin
          let res_t = find_valuation (transform T id cnf) ((id, T)::partial_sol) (id + 1)
          and res_f = find_valuation (transform F id cnf) ((id, F)::partial_sol) (id + 1)
          in 
          if res_t <> None then res_t else res_f
        end
    in
    find_valuation cnf [] 0
end
