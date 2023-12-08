let trim str =
  if str = "" then "" else
  let search_pos init p next =
    let rec search i =
      if p i then raise (Failure "empty") else
      match str.[i] with
      | ' ' | '\n' | '\r' | '\t' -> search (next i)
      | _ -> i
    in search init
  in
  let len = String.length str in
  try
    let left = search_pos 0 (fun i -> i >= len) (succ)
    and right = search_pos (len - 1) (fun i -> i < 0) (pred)
    in String.sub str left (right - left + 1)
  with Failure _ -> ""

let rec range a b step = 
  if a > b then [] else a :: range (a + step) b step

let rec count lst pred =
  match lst with
  | [] -> 0
  | x :: xs -> (if pred x then 1 else 0) + count xs pred

let () = 
  let xs = read_line() 
    |> String.split_on_char '+'
    |> List.map (fun x -> int_of_string @@ trim x) 
  in
  
  let x_max = List.fold_left max 0 xs in
  let conjugate =
    range 1 x_max 1
    |> List.map (fun i -> string_of_int @@ count xs (fun x -> x >= i))
    |> String.concat " + "
  in
  print_endline conjugate
  