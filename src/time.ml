

(** obtains the current time in milliseconds *)
let now = fun () -> int_of_float ( ( Sys.time () ) *. 1000.0 )
