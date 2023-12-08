let divs from_k n =
  let rec aux d n = 
    if d * d > n then []
    else if n mod d = 0 then d :: n / d :: aux (d+1) n else aux (d+1) n
  in aux from_k n |> List.sort compare

let isprime n = n >= 2 && divs 2 n = []

let isgauss x y =
  if x == 0 then y mod 4 = 3 && isprime y
  else if y == 0 then x mod 4 = 3 && isprime x
  else isprime @@ x * x + y * y

let () = 
  let parse x y = (x, y) in
  let n = int_of_string (input_line stdin) in
  for i = 0 to n - 1 do
    let x, y = Scanf.sscanf (read_line()) "%d %d" parse in
    print_endline @@ string_of_bool @@ isgauss x y;
  done;
