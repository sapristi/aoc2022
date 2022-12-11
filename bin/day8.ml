let file = "./inputs/day8"

let lines = Aoc.read_lines file

let test_lines = [
  "30373";
  "25512";
  "65332";
  "33549";
  "35390";
]

let parse_line l =
  String.to_seq l |> Seq.map int_of_char |> Array.of_seq

let make_forest lines = List.map parse_line lines |> Array.of_list


module Coord = struct
  type t = (int*int)
  let compare = compare
end
module CoordSet = Set.Make(Coord)

(* let all_coords = Seq.product (Seq.init height (fun x -> x)) (Seq.init width (fun x -> x)) *)
(* let visible = ref (CoordSet.add_seq all_coords CoordSet.empty);; *)


module Part1 = struct
  let find_visibles forest =

    let visible = ref CoordSet.empty
    and height = Array.length forest
    and width = Array.length (forest.(0)) in
    for i = 0 to height -1 do
      let biggest_seen = ref (-1) in
      for j = 0 to width -1 do
        let tree = forest.(i).(j) in
        if tree > (!biggest_seen)
        then visible := CoordSet.add (i,j) (!visible);
        biggest_seen := Int.max tree (!biggest_seen)
      done;
    done;

    for i = 0 to height -1 do
      let biggest_seen = ref (-1) in
      for j = 0 to width -1 do
        let tree = forest.(i).(width -1 - j) in
        if tree > (!biggest_seen)
        then visible := CoordSet.add (i,width -1 - j) (!visible);
        biggest_seen := Int.max tree (!biggest_seen)
      done;
    done;

    for j = 0 to width -1 do
      let biggest_seen = ref (-1) in
      for i = 0 to height -1 do
        let tree = forest.(i).(j) in
        if tree > (!biggest_seen)
        then visible := CoordSet.add (i,j) (!visible);
        biggest_seen := Int.max tree (!biggest_seen)
      done;
    done;

    for j = 0 to width -1 do
      let biggest_seen = ref (-1) in
      for i = 0 to height -1 do
        let tree = forest.(height -1 - i).(j) in
        if tree > (!biggest_seen)
        then visible := CoordSet.add (height - 1 - i,j) (!visible);
        biggest_seen := Int.max tree (!biggest_seen)
      done;
    done;
    !visible

  let solution lines =
    let forest = make_forest lines in
    let visible = find_visibles forest in
    visible

end

module Part2 = struct

  let find_vision_north forest i j =
    if i = 0 then 0
    else
      let k = ref (i-1) in
      while forest.(!k).(j) < forest.(i).(j) && (!k) > 0 do
        decr k;
      done;
      let res =  i - (!k) in
      Printf.printf "North: %d -> %d : %d\n" i (!k) res;
      res

  let find_vision_south forest i j =
    let height = Array.length forest in
    if i = height - 1 then 0
    else
      let k = ref (i+1) in
      while forest.(!k).(j) < forest.(i).(j) && (!k) < height -1 do
        incr k
      done;
      let res = (!k) - i in
      Printf.printf "South: %d -> %d : %d\n" i (!k) res;
      res

  let find_vision_west forest i j =
    if j = 0 then 0
    else
      let k = ref (j-1) in
      while forest.(i).(!k) < forest.(i).(j) && (!k) > 0 do
        decr k
      done;
      let res = j - (!k) in 
      Printf.printf "West: %d -> %d : %d\n" j (!k) res;
      res

  let find_vision_east forest i j =
    let width = Array.length (forest.(0)) in
    if j = width -1 then 0
    else
      let k = ref (j+1) in
      while forest.(i).(!k) < forest.(i).(j) && (!k) < width -1 do
        incr k
      done;
       let res =  (!k) - j in
       Printf.printf "East: %d -> %d : %d\n" j (!k) res;
       res

  let find_vision forest (i, j) =
    Printf.printf "Find vision %d,%d\n" i j;
    (find_vision_east forest i j ) * (find_vision_west forest i j ) *
    (find_vision_north forest i j)  * (find_vision_south forest i j)

  let find_visions forest =

    let height = Array.length forest in
    let width = Array.length (forest.(0)) in
    let all_coords = Seq.product (Seq.init (height-2) (fun x -> x+1)) (Seq.init (width-2) (fun x -> x+1)) in
    Seq.map (find_vision forest) all_coords |> List.of_seq

  let solution lines =
    let forest = make_forest lines in
    let visions = find_visions forest in
    List.fold_left (fun res item -> Int.max res item) 0 visions 
end

let () =
  Printf.printf "\nVisible %d\n" (CoordSet.cardinal (Part1.solution lines));
  Printf.printf "\nVisible %d\n" (Part2.solution lines);
