
let rec get_sum s =
  match s with
  | [] -> 0
  | hd :: tl -> (Char.code hd) + get_sum tl

let rec solve sum s = 
  match s with
  | [] -> "prime"
  | hd :: tl -> 
    if sum mod (Char.code hd) = 0 then (String.make 1 hd)
    else solve sum tl

let () =
  let s = read_line() |> String.to_seq |> List.of_seq in
  let char_sum = get_sum s in
  print_endline (solve char_sum s)
