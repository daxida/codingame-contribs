let tochr s = s |> String.to_seq |> List.of_seq |> List.hd

let () =
  let n = read_int()
  and c = tochr @@ read_line()
  and s = read_line() |> String.split_on_char ' ' in

  let needed = 
    List.init (n-1) (fun i -> String.make (i+1) c)
    |> List.filter (fun x -> not (List.mem x s)) 
    |> String.concat " "
  in

  print_endline @@ 
    if String.length needed = 0 then string_of_int n 
    else needed;
