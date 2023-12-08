let process_votes timeout n =
  let time_record = Hashtbl.create 101 in
  let tally = Array.init 2 (fun _ -> 0) in
  
  let process_vote (user_id, vote, time) =
    let prev = try Hashtbl.find time_record user_id with Not_found -> -200 in
    if time - prev >= timeout then begin
      tally.(vote) <- tally.(vote) + 1;
      Hashtbl.add time_record user_id time
    end
  in
  
  List.init n (fun _ -> Scanf.sscanf (read_line()) "%d %d %d" (fun a b c -> (a, b, c)))
  |> List.iter process_vote;
  
  tally

let () = 
  let timeout = read_int() in
  let n = read_int() in
  let result = process_votes timeout n in
  Printf.printf "%d %d\n" result.(0) result.(1);
