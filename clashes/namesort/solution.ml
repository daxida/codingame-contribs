let to_char_list str = str |> String.to_seq |> List.of_seq

let rec find x lst =
  match lst with
  | [] -> raise (Failure "Not Found")
  | h :: t -> if x = h then 0 else 1 + find x t

let () = 
  let lst = "854917632" |> to_char_list in 
  
  read_line()
    |> to_char_list
    |> List.sort (fun a b -> compare (find a lst) (find b lst))
    |> List.map (fun c -> String.make 1 c)
    |> String.concat ""
    |> print_endline
