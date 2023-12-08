let rec repl n s = if n = 1 then s else s ^ repl (n - 1) s

let rec replace (lst : string list) : string list =
  match lst with
  | [] -> []
  | h::t -> (if h = "0" then "" else h) :: replace t

let group (n : int) (c : string) : string list =
  let rec aux n c =
    match n with
    | 0 -> [""]
    | n -> repl n c :: aux (n-1) c
  in List.rev @@ aux n c

let () =
  let n = read_int()
  and c = read_line()
  and s = read_line() |> String.split_on_char ' ' |> replace in

  let sorted = List.sort compare s in
  let group = group (n - 1) c in

  if group = sorted then print_int n else
    group
    |> List.filter (fun c -> not @@ List.mem c sorted)
    |> String.concat " "
    |> print_endline
