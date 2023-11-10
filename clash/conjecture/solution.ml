let solve n s =
  let sum = ref (List.fold_left (+) 0 s) in
  let length = ref n in
  while (!sum / !length) <> (!sum mod !length) do
    sum := !sum + (!sum mod !length);
    length := !length + 1;
  done;
  !sum mod !length
;;

let () =
  let n = int_of_string (read_line ()) in
  let s =
    read_line ()
    |> String.split_on_char ' '
    |> List.map int_of_string
  in
  print_int @@ solve n s
;;
