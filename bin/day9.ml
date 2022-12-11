let file = "./inputs/day9" 

let lines = Aoc.read_lines file

let test_lines = [
  "R 4";
  "U 4";
  "L 3";
  "D 1";
  "R 4";
  "D 1";
  "L 5";
  "R 2";
]

module Coord = struct
  type t = (int*int)

  let add (x1, y1) (x2, y2) = (x2 + x1, y2 + y1)
  let diff (x1, y1) (x2, y2) = (x1 - x2, y1 - y2)

  let norm (x,y) = Int.max (Int.abs x) (Int.abs y)

  let compare = compare
end

let simp value = if value = 0 then 0 else if value > 0 then 1 else -1

module Rope = struct
  type t = {
    head: Coord.t;
    tail: Coord.t;
  }


  let get_move rope =
    let dx, dy = Coord.diff rope.head rope.tail in
    if Coord.norm (dx,dy) <= 1
    then (0, 0)
    else
      if dx = 0 then (0, simp dy)
      else if dy = 0 then (simp dx, 0)
      else (simp dx, simp dy)

  let apply_move rope move =
    let new_head = Coord.add rope.head move in
    let tail_move = get_move {head=new_head; tail=rope.tail}
    in let new_tail = Coord.add rope.tail tail_move in
    {
      head = new_head;
      tail = new_tail
    }
end

let parse_line line =
  let value = CCString.drop 2 line |> int_of_string in
  if line.[0] = 'R' then
    Seq.init value (fun _ -> (0,1))
  else if line.[0] = 'L' then
    Seq.init value (fun _ -> (0,-1))
  else if line.[0] = 'U' then
    Seq.init value (fun _ -> (1,0))
  else if line.[0] = 'D' then
    Seq.init value (fun _ -> (-1,0))
  else
    failwith "Bad input"

let parse_lines lines =
  Seq.concat (lines |> List.map parse_line |> List.to_seq )

module CoordsSet = Set.Make(Coord)
module Part1 = struct

  let solution lines =
    let rope  = ref {Rope.head= (0,0); tail=(0,0)} in
    let tail_values = ref (CoordsSet.singleton (!rope).tail) in
    let insts = parse_lines lines in
    Seq.iter (fun inst ->
        rope := Rope.apply_move (!rope) inst;
        let {Rope.head=(x1, y1); tail=(x2,y2)} = (!rope) in 
        Printf.printf "(%d, %d) | (%d, %d)\n" x1 y1 x2 y2;
        tail_values := CoordsSet.add (!rope).tail (!tail_values)
      ) insts;
    Printf.printf "Part1 %d\n" ((!tail_values) |> CoordsSet.cardinal)
end

module Part2 = struct

  type t = Coord.t array


  let get_move rope i =
    let dx, dy = Coord.diff rope.(i) rope.(i+1) in
    if Coord.norm (dx,dy) <= 1
    then (0, 0)
    else
    if dx = 0 then (0, simp dy)
    else if dy = 0 then (simp dx, 0)
    else (simp dx, simp dy)

  let apply_move rope move =
    rope.(0) <- Coord.add rope.(0) move;
    for i = 0 to (Array.length rope - 2) do
      let move_i = get_move rope i in
      rope.(i+1) <- Coord.add rope.(i+1) move_i
    done


  let solution lines =
    let rope  = Array.make 10 (0,0) in
    let tail_values = ref (CoordsSet.singleton (0,0)) in
    let insts = parse_lines lines in
    Seq.iter (fun inst ->
        apply_move rope inst;
        (* let {Rope.head=(x1, y1); tail=(x2,y2)} = (!rope) in  *)
        (* Printf.printf "(%d, %d) | (%d, %d)\n" x1 y1 x2 y2; *)
        tail_values := CoordsSet.add rope.(9) (!tail_values)
      ) insts;
    Printf.printf "Part2 %d\n" ((!tail_values) |> CoordsSet.cardinal)


end

let () =
  Part1.solution lines;
  Part2.solution lines;
