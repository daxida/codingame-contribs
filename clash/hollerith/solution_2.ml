let chunk_lengths n s : (int list) = 
  let sz = String.length s in
  let full = sz / n
  and rest = sz mod n in
  List.init full (fun _ -> n) @ (if rest > 0 then [rest] else [])

let chunkify n s : (string list) = 
  let cl = chunk_lengths n s in
  List.fold_left 
    (* acc buffers the chunks, i stores the starting length *)
    (fun (acc, i) l -> ((String.sub s i l)::acc, i+l)) ([], 0) cl
  |> fun (acc, _) -> List.rev acc

let hollerith (n : int) (s : string) : string =
  chunkify n s
  (* Prepend size + H *)
  |> List.map (fun c -> (string_of_int (String.length c)) ^ "H" ^ c)
  (* Prepend and append "/" plus concat at "," *)
  |> (fun cs -> "/" ^ (String.concat "," cs) ^ "/")

let () = 
  let n = read_int () 
  and s = read_line() in
  print_endline @@ hollerith n s