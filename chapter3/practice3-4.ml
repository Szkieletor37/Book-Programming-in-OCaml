let geo_mean (x, y) =
    sqrt(x *. y);;

let bmi (name, height, weight) =
    let bmi_index = weight /. (height *. height) in 
    if bmi_index < 18.5 then name ^ " is thin." else
    if bmi_index >= 18.5 && bmi_index < 25. then name ^ " is ordinary." else
    if bmi_index >= 25. && bmi_index < 30. then name ^ " is fat." else
    name ^ " is extremely fat.";;

let f (sum, dif) =
    (((sum +. dif) /. 2.), ((sum -. dif) /. 2.));;

let sum_and_diff (x, y) =
    ((x +. y), (x -. y));;