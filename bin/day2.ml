let file = "./inputs/day2";;

let read_lines filename = 
  let lines = ref [] in
  let chan = open_in filename in
  try
    while true; do
      lines := input_line chan :: !lines
    done; !lines
  with End_of_file ->
    close_in chan;
    List.rev !lines

type move = Rock | Paper | Scisor

let match_score opp own =
  if opp == own then 3
  else (
    match opp, own with
    | Rock, Paper -> 6
    | Paper, Scisor -> 6
    | Scisor, Rock -> 6
    | _ -> 0
  )

let move_score m = match m with | Rock -> 1 | Paper -> 2 | Scisor -> 3

module Part1 = struct

  let move_from_char_opp c =
    match c with
    | 'A' -> Rock
    | 'B' -> Paper
    | 'C' -> Scisor
    | _ -> failwith "unknown move"

  let move_from_char_own c =
    match c with
    | 'X' -> Rock
    | 'Y' -> Paper
    | 'Z' -> Scisor
    | _ -> failwith "unknown move"
  let score_from_string s =
    let m_opp = move_from_char_opp s.[0] and m_own = move_from_char_own s.[2] in
    (match_score m_opp m_own) + (move_score m_own)
end


let part1 () =
  let input = read_lines file in
  let res = List.fold_left (+) 0 (List.map Part1.score_from_string input) in
  Printf.printf "Res %d\n" res


module Part2 = struct

  type result = | Win | Draw | Loose

  let result_from_char c = match c with
    | 'X' -> Loose | 'Y' -> Draw | 'Z' -> Win | _ -> failwith "unknown result"

  let rec compute_own_move opp res =
    match opp, res with
    | _, Draw -> opp
    | Paper, Win -> Scisor
    | Rock, Win -> Paper
    | Scisor, Win -> Rock
    | Paper, Loose -> Rock
    | Rock, Loose -> Scisor
    | Scisor, Loose -> Paper

  let move_from_char_opp c =
    match c with
    | 'A' -> Rock
    | 'B' -> Paper
    | 'C' -> Scisor
    | _ -> failwith "unknown move"

  let score_from_string s =
    let m_opp = move_from_char_opp s.[0] and result = result_from_char s.[2] in
    let m_own = compute_own_move m_opp result in
    (match_score m_opp m_own) + (move_score m_own)

end

let part2 () =
  let input = read_lines file in
  let res = List.fold_left (+) 0 (List.map Part2.score_from_string input) in
  Printf.printf "Res %d\n" res

let () = part2 ()
