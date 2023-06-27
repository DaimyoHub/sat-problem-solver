type lit_t
type clause_t
type cnf_t
type valuation_t

type tri_t


module Clause :
sig
  val find_and_remove_positive : int -> clause_t -> clause_t
  val find_and_remove_negative : int -> clause_t -> clause_t

  val transform : tri_t -> int -> clause_t -> clause_t
  val substitute_and_transform : tri_t -> int -> clause_t -> clause_t

  val evaluate : clause_t -> tri_t

  val find_max_id : clause_t -> int
end


module Cnf :
sig
  val find_satisfying_valuation : cnf_t -> valuation_t option

  val transform : tri_t -> int -> cnf_t -> cnf_t

  val evaluate : cnf_t -> tri_t

  val max_literal_id : cnf_t -> int
end