let file = "./inputs/day6"

let lines = Aoc.read_lines file


let test_lines = [
  "mjqjpqmgbljsphdztnvjfqwrcgsmlb";
]
module Part1 = struct

  module CS = Set.Make(Char)

  let nb_different_chars (s:string) =
    CS.add_seq (String.to_seq s) CS.empty |> CS.cardinal

  exception Result of int

  let solution lines =
    let line = List.hd lines in

    try
      for i=0 to (String.length line) -5 do
        if nb_different_chars (String.sub line i 4) = 4
        then
          raise (Result (i+4))
      done;
      failwith "No result"
    with Result res -> res
end

module Part2 = struct


  let solution lines =
    let line = List.hd lines in

    try
      for i=0 to (String.length line) - 15 do
        if Part1.nb_different_chars (String.sub line i 14) = 14
        then
          raise (Part1.Result (i+14))
      done;
      failwith "No result"
    with Part1.Result res -> res

end

let () =
  Printf.printf "\nPART1 %d\n " (Part1.solution lines);
  Printf.printf "\nPART2 %d\n " (Part2.solution lines)
