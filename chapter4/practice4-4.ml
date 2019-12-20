(* 4.1 *)
let uncurry f (x, y) = f x y;;

(* 4.2 *)
let rec repeat f n x =
    if n > 0 then repeat f (n - 1) (f x) else x;;

let fib n =
    let (fibn, _) = repeat (fun (x, y) -> (x + y, x)) n (1, 0)
    in fibn;;

let s x y z = x z (y z);;

let k x y = x;;

let i = s k k;;

(* fun x y -> y *)
let skifalse = k i;;