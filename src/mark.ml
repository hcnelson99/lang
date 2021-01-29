type pos = Lexing.position
type 'a t = 'a * ((pos * pos)[@sexp.opaque]) [@@deriving sexp]

let map (obj, range) ~f = f obj, range
let start (_, (start, _)) = start
let stop (_, (_, stop)) = stop
let obj (obj, _) = obj
let with_mark mark obj = map ~f:(fun _ -> obj) mark
let create obj start stop = obj, (start, stop)
let with_range b e obj = create obj (start b) (stop e)
