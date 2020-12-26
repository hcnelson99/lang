open Core

module MkVar () : Var_intf.S = struct
  module T = struct
    type t = int [@@deriving sexp, compare, hash, equal]
  end

  include T
  include Comparable.Make (T)
  include Hashable.Make (T)

  let next_id = ref 0

  let create () =
    let res = !next_id in
    next_id := res + 1;
    res
  ;;

  let to_string = Int.to_string
end

include MkVar ()
