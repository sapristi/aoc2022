let file = "./inputs/day3";;

let lines = Aoc.read_lines file

let ex_lines = [
    "vJrwpWtwJgWrhcsFMMfFFhFp";
    "jqHRNqRjqzjGDLGLrsFMfFZSrLrFZsSL";
    "PmmdzqPrVvPwwTWBwg";
    "wMqvLMZHhHMvwLHjbvcjnnSBnvTQFn";
    "ttgJtRGJQctTZtZT";
    "CrZsJsPPZsGzwwsLwLmpwMDw"
]

module CS = Set.Make(Char)

let priority c =
  let value = int_of_char c in
  if value >= 97 then value - 96
  else value - 38

let make_set s =
  String.fold_right CS.add s CS.empty

module Part1 = struct

  let dup_item line =
    let half_length = (String.length line) / 2 in
    let s1 = make_set (String.sub line 0 half_length) and s2 = make_set (String.sub line half_length half_length) in
    let inter = CS.inter s1 s2 in
    CS.choose inter

  let solution lines =
    (* List.iter (fun line -> let dup = dup_item line in let p = priority dup in Printf.printf "%c -> %d\n" dup p) lines; *)
    List.fold_left (+) 0 (List.map priority (List.map dup_item lines))
end


module Part2 = struct

  let rec compute_badges lines res =
    match lines with
    | [] -> res
    | l1::l2::l3::t ->
      let inter = CS.inter (make_set l1) (CS.inter (make_set l2) (make_set l3) ) in
      let badge = CS.choose inter in
       compute_badges t (badge::res)
    | _ -> failwith "unexpected lines"

  let solution lines =
    List.fold_left (+) 0 (List.map priority (compute_badges lines []))

end

let () = let res = Part2.solution lines
             in Printf.printf "res %d\n" res
