open Core

module Make () : Named_var_intf.S = struct
  module Id = Uid.Make ()

  module T = struct
    type t = string * Id.t [@@deriving hash, sexp, compare]
  end

  include T
  include Hashable.Make (T)
  include Comparable.Make (T)

  let create n = n, Id.create ()
  let to_string (n, id) = n ^ "#" ^ Id.to_string id
  let to_string_hum (n, _) = n
  let name (n, _) = n
end
