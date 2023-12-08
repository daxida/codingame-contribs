(* DEBUG *)
let ppa arr =
  Array.iter (fun inner_arr ->
    Array.iter (fun elem -> 
      output_string stderr (string_of_int elem);
      output_string stderr " "
    ) inner_arr;
    output_string stderr "\n"
  ) arr

let time f =
  let fx = f in
  Printf.eprintf "Time: %fs\n" @@ Sys.time();
  fx

(* START *)
let inf = 100000

let rec gcd a b =
  if b = 0 then a else gcd b (a mod b)

let verify lst =
  let drop_last = List.rev @@ List.tl @@ List.rev lst in
  List.for_all2 (fun x y -> gcd x y == 1) drop_last (List.tl lst)

let held_karp n xs = 
  let n = n + 1 in (* Universal point *)
  
  (* Compute distances and initialize dp *)
  let dist = Array.make_matrix n n 0 in
  for i = 0 to n - 2 do
    for j = 0 to n - 2 do
      if i <> j then
        let g = gcd (List.nth xs i) (List.nth xs j) in
        dist.(i + 1).(j + 1) <- if g = 1 then 1 else 2
    done
  done;

  let dp = Array.make_matrix n (1 lsl n) inf in
  for i = 0 to n - 1 do
    dp.(i).(1 lsl i) <- dist.(0).(i)
  done;

  ppa dist;
  ppa dp;

  (* Held-Karp main *)
  for i = 0 to (1 lsl n) - 1 do
    if i land 1 <> 0 then
      for j = 0 to n - 1 do
        if dp.(j).(i) <> inf then
          for k = 0 to n - 1 do
            let nxt = dp.(j).(i) + dist.(j).(k) in
            let mask = i lor (1 lsl k) in
            dp.(k).(mask) <- min dp.(k).(mask) nxt
          done
      done
  done;
  
  (* Backtrack an optimal TSP tour *)
  let backtrack = 
    let mask = (1 lsl n) - 1 in
    let cur = ref 0 in

    let rec backtrack' mask cur acc =
      if mask <= 1 then List.rev acc 
      else
        let nxt = ref None in
        let min_dist = ref inf in
    
        for i = 1 to n - 1 do
          if mask land (1 lsl i) <> 0 && dp.(i).(mask) + dist.(i).(cur) < !min_dist then 
            begin
              min_dist := dp.(i).(mask) + dist.(i).(cur);
              nxt := Some i
            end
        done;
    
        match !nxt with
        | Some nxt ->
          let new_mask = mask lxor (1 lsl nxt) in
          backtrack' new_mask nxt (acc @ [nxt])
        | None -> begin
            print_endline "Path not found";
            exit 0
          end
    in 
    backtrack' mask !cur []
  in 
  List.map (fun idx -> List.nth xs (idx - 1)) backtrack

let main =
  let n = read_int () in
  let xs = read_line () 
    |> String.split_on_char ' '
    |> List.map int_of_string 
    |> List.sort compare in

  let path = held_karp n xs in
  
  (* Print solution if any *)
  if (verify path) then
    path 
      |> List.rev 
      |> List.map string_of_int 
      |> String.concat " " 
      |> print_endline
  else
    print_endline "-1"

let () = 
  time main