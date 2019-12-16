let rec pow (x, n) =
    if n = 1 then x else x *. pow(x, (n - 1));;

let rec pow2 (x, n) =
    let is_even n = if n mod 2 = 0 then true else false in
    if (is_even n)
        then pow (x, (n / 2)) *. pow (x, (n / 2))
        else x *. pow (x, (n / 2)) *. pow (x, (n / 2));;

let pow3 (x, n) = 
    let rec iterpow (x, res, n) =
        if n = 1 then res
        else iterpow (x, res *. x, n - 1) in
    iterpow (x, 1.0, n);;

let rec gcd (x, y) =
    if x < y then gcd (y, x)
    else if x mod y = 0 then y
    else
        let tmp = y in
        let next_y = x mod y in
        let next_x = tmp in
        gcd (next_x, next_y);;

let rec comb (n, m) =
    if m = 0 then 1
    else if n > m then comb (n-1, m) + comb (n-1, m-1) else comb (n-1, m-1);;

let iterfib n =
    let rec fibtail (s, t, i) = 
        if i = n
            then s
            else fibtail (t, s+t, i+1) in
    fibtail (1, 1, 1);;

let max_ascii str =
    let len = String.length str in
    let rec max_ascii_iter (str, iter, current_max_ascii_char) =
        if iter = len
            then current_max_ascii_char
            else if str.[iter] > current_max_ascii_char
                then max_ascii_iter(str, iter+1, str.[iter])
                else max_ascii_iter(str, iter+1, current_max_ascii_char) in
    max_ascii_iter (str, 0, ' ');;

let rec pos n =
        if n < 0
            then 0.0
            else pos (n-1) -. 1.0 /. (float_of_int (4 * n + 3)) +. 1.0 /. (float_of_int (4 * n + 1));;
