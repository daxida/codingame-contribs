
(* Utility function: same as python startswith *)
let startswith word sentence = 
  let word_len = String.length word in
  (word_len <= String.length sentence) && String.sub sentence 0 word_len = word

(* Utility function: removes the first occurence of "item" from the list *)
let remove_item item lst =
  let rec aux lst item newlst =
    match lst with
      | [] -> List.rev newlst
      | hd :: tl ->
      if hd = item then List.rev_append newlst tl
      else aux tl item (hd :: newlst)
  in
  aux lst item []

let find_all_solutions original words =
  let solutions = ref [] in

  let rec solve original words solution =
    match original, words with
      | "", [] -> 
        (* Here we found a solution. We have no "original" nor "words" left to decypher *)
        let sol = String.concat " " (List.rev solution) in
        solutions := sol :: !solutions;
        (* More than one solution means unsolvable *)
        if List.length !solutions > 1 then begin
          print_endline "Unsolvable";
          exit 0
        end
      | _ ->
        let set_words = List.sort_uniq compare words in
        (* We only iterate over the set of words to avoid duplicates and extra iterations *)
        List.iter (fun word -> 
          (* If it matches the beggining of "original" we try the current word *)
          if startswith word original then
            let word_len = String.length word in
            let ori_len = String.length original in
            let next_ori = String.sub original word_len (ori_len - word_len) in
            (* We pass a copy of "words" where we removed only one instance of the chosen word *)
            let new_words = remove_item word words in
            solve next_ori new_words (word :: solution)
        ) set_words
  in
  
  solve original words [];
  !solutions

let () = 
  let original = read_line () in
  let words = read_line() |> String.split_on_char ' ' in
  print_endline (List.hd (find_all_solutions original words))

