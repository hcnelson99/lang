(* row, col *)
type pos = Lexing.position

type 'a t =
  { obj : 'a
  ; start : pos
  ; stop : pos
  }

let map ({ obj; _ } as m) ~f = { m with obj = f obj }
let start { start; _ } = start
let stop { stop; _ } = stop
let with_mark mark obj = map ~f:(fun _ -> obj) mark
let create obj start stop = { obj; start; stop }
let with_range b e obj = create obj (start b) (stop e)
let obj { obj; _ } = obj
