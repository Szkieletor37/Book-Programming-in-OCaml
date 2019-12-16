let my_and b1 b2 =
    if b1
        then
            (if b2 then true else false)
        else false;;

let my_or b1 b2 =
    if b1
        then true
        else (if b2 then true else false);;

let my_and2 b1 b2 =
    if b1 then b2 else false;;

let my_or2 b1 b2 =
    if b1 then true else b2;;

let my_and3 b1 b2 =
    not((not b1) || (not b2));;

let my_or3 b1 b2 =
    not((not b1) && (not b2));;
