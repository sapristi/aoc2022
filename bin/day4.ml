let file = "./inputs/day4";;

let lines = Aoc.read_lines file

let test_lines = [
  "2-4,6-8";
  "2-3,4-5";
  "5-7,7-9";
  "2-8,3-7";
  "6-6,4-6";
  "2-6,4-8";
]

module Range = struct

  type t = (int * int)

  let contains (min1, max1) (min2, max2) =
    min1 <= min2 && max1 >= max2


  let overlap_bound (min1, max1) (min2, max2) =
    (min1 <= min2 && max1 >= min2) ||
    (min1 <= max2 && max1 >= max2)

  let overlap part1 part2 =
    overlap_bound part1 part2 ||
    contains part1 part2 ||
    contains part2 part1

end


let parse_line line =
  let parse_part part =
    let [b1; b2] = String.split_on_char '-' part in
    (Int64.of_string b1, Int64.of_string b2)
  in
  let [part1; part2] = String.split_on_char ',' line in
  (parse_part part1, parse_part part2)



module Part1 = struct

  let solution lines =
    let parsed_lines = List.map parse_line lines in
    let res = ref 0 in
    List.iter (fun (part1,part2) -> if Range.contains part1 part2 || Range.contains part2 part1 then res := !res +1) parsed_lines;
    Printf.printf "\nPart1 %d\n" !res
end

module Part2 = struct

  let fpart (m1,m2) = Printf.sprintf "%Ld-%Ld" m1 m2
  let solution lines =
    let parsed_lines = List.map parse_line lines in
    let res = ref 0 in
    List.iter (
      fun (part1,part2) ->
        if Range.overlap part1 part2
        then
          res := !res +1
        else
          Printf.printf "Not overlap %s %s\n" (fpart part1) (fpart part2)
    ) parsed_lines;
    Printf.printf "\nPart2 %d\n" !res
end

let () =
  Part1.solution lines;
  Part2.solution lines
