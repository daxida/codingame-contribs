let trans c =
  match c with
  | 'b' -> 'q' | 'q' -> 'b'
  | 'd' -> 'p' | 'p' -> 'd'
  | 'u' -> 'n' | 'n' -> 'u'
  |  c -> c

let () = 
  input_line stdin
  |> String.to_seq
  |> List.of_seq
  |> List.map trans
  |> List.rev
  |> List.iter (fun c -> Printf.printf "%c" c)