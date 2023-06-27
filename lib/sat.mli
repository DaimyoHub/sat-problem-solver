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


(** A literal is theorically represented by either a simple propositional
    variable 'x' or a formula of the form 'not(x)'. 

    In practice, we also add two literal types of the form Tlit and Flit, which
    respectively represent the truth value and the falsity value. It is used
    to represent a clause which is either evaluated to true or to false. *)
type lit_t

(** A clause is a disjunction of literals. *)
type clause_t

(** A CNF formula is a conjunction of clauses. *)
type cnf_t

(** The valuation associates each literal with a truth value. *)
type valuation_t

(** The tri_t type allows us to partially evaluate a formula. *)
type tri_t


module Clause :
sig
  (** [search_and_remove_positive] finds and removes all occurences of V([id]) in
      the given clause [c]. *)
  val search_and_remove_positive : int -> clause_t -> clause_t

  (** Same as [search_and_remove_positive] but instead, it works on literals of
      the form Nv([id]). *)
  val search_and_remove_negative : int -> clause_t -> clause_t

  (** [transform] substitutes eache occurences of V([id]) ou Nv([id]) with the
      value [t], then it evaluates it to simplify the given formula [c]. *)
  val transform : tri_t -> int -> clause_t -> clause_t

  (** Same as [transform], but treats the invalid case [t] = U. *)
  val substitute_and_transform : tri_t -> int -> clause_t -> clause_t

  (** [evaluate] evaluates the given clause [c] to a trilean value. *)
  val evaluate : clause_t -> tri_t

  (** [find_max_id] finds the maximum id contained by the given clause [c]. *)
  val find_max_id : clause_t -> int
end


module Cnf :
sig
  (** [search_satisfying_valuation] searches a valuation making the given CNF
      formula [cnf] true when applying it.
      
      The algorithm principle is based on the Quine-McCluskey algorithm :

      until the partial solution does not satify the whole formula
        for each literal
          on the one hand
            - substitute it with True to transform the formula
            - associate the given literal with True on the partial solution
            - recursively call the algorithm on the transformed formula with
              the partial solution
          on the second hand
            - substitute it with False to transform the formula
            - associate the given literal with True on the partial solution
            - recursively call the algorithm on the transformed formula with
              the partial solution *)
  val search_satisfying_valuation : cnf_t -> valuation_t option

  (** [transform] transforms each clause of [id] contained by [cnf] with the
      given trilean value [t]. *)
  val transform : tri_t -> int -> cnf_t -> cnf_t

  (** [evaluate] evaluates the given formula [cnf] to a trilean value. *)
  val evaluate : cnf_t -> tri_t

  (** [find_max_id] finds the maximum id among all clauses contained by [cnf]. *)
  val find_max_literal_id : cnf_t -> int
end