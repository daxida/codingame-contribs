open String

let rec chunkify n s i (acc : string list) : (string list) =
  if i >= length s then List.rev acc
  else
    let j = min (i + n) (length s) in
    let c = sub s i (j - i) in
    chunkify n s j (c :: acc)

let hollerith (n : int) (s : string) : (string) =
  let chunks = chunkify n s 0 [] in
  chunks
  (* Preprend size + H *)
  |> List.map (fun c -> (string_of_int (length c)) ^ "H" ^ c)
  (* Prepend and append "/" plus concat at "," *)
  |> (fun cs -> "/" ^ (concat "," cs) ^ "/")

let () = 
  let n = read_int () 
  and s = read_line() in
  print_endline @@ hollerith n s
