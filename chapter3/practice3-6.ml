let rec pow_curry n x =
    if n = 1 then x else x * (pow_curry (n-1) x);;

let cube = pow_curry 3;;

let rec pow_curry2 x n =
    if n = 1 then x else x * (pow_curry2 x (n-1));;

let cube2 = fun x -> pow_curry2 x 3;;

let integral f a b =
    let delta = (b -. a) /. 1.0e3 in
    let rec sum_of_trapezoidal_approximation f i =
        let i_float = float_of_int i in
        if a +. delta *. i_float > b
            then 0.0
            else (f (a +. (i_float -. 1.0) *. delta) +. f (a +. i_float *. delta)) *. delta /. 2.0 +. sum_of_trapezoidal_approximation f (i+1) in
    sum_of_trapezoidal_approximation f 1;;

let answer = 
    let pi = 3.14 in
    integral sin 0.0 pi;;