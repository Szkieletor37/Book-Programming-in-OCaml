let round_off x =
    if int_of_float (x *. 10.0) mod 10 >= 5
        then (int_of_float x) + 1
        else (int_of_float x);;

let dollar_to_yen dollar =
    round_off (dollar *. 114.32);;

let yen_to_dollar yen = 
    round_off ((float_of_int yen) /. 114.32);;

let return_dollar_to_yen_strings dollar =
    let yen = dollar_to_yen dollar in
    (string_of_float dollar) ^ " dollars are " ^ (string_of_int yen) ^ " yen.";;

let capitalize char =
    if char >= 'a' && char <= 'z' 
        then (char_of_int ((int_of_char char) - 32)) 
        else char;;