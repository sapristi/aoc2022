let file = "./inputs/day10" 

let lines = Aoc.read_lines file
let test_lines = Aoc.read_lines "./inputs/day10_test"

type inst = | Noop | Add of int

let parse_line line =
  if line = "noop" then Noop else
    Add (CCString.drop 5 line |> int_of_string)

let is_target_cycle n =
  (n - 20) mod 40 = 0 && n <= 220

module ToApply = struct
  type t = (int*int) option
  let dec v =
    Option.map (fun (value, time) -> (value, time -1)) v
end

module Part1 = struct
let execute insts =
  let res = ref 0 in
  let reg = ref 1 in

  let rec make_cycle insts cycle to_add =
    if cycle > 222 then ()
    else
    if is_target_cycle cycle then
      (
      Printf.printf "Cycle %d: %d\n" cycle (!reg);
      res := (!res) + cycle * (!reg)
    );
    match to_add with
    | Some (0, value) ->
      reg := (!reg) + value;
      make_cycle insts (cycle+1) None
    | Some (i, value) ->
      make_cycle insts (cycle+1) (Some (i-1, value))
    | None ->
      match insts with
      | Noop :: insts' -> make_cycle insts' (cycle+1) None
      | Add(n):: insts' -> make_cycle insts' (cycle+1) (Some (0, n))
      | [] -> ()


  in 
  make_cycle insts 1 None;
  !res

end

module Part2 = struct

  let is_pixel_lighted cycle register =
    let sprite_positions = [register; register +1; register+2] in
    List.mem (cycle mod 40) sprite_positions

  let execute insts =
    let reg = ref 1 in

    let rec make_cycle insts cycle to_add =
      if is_pixel_lighted cycle !reg
      then print_string "#"
      else print_string ".";
      if cycle mod 40 = 0
      then print_endline "";
      match to_add with
      | Some (0, value) ->
        reg := (!reg) + value;
        make_cycle insts (cycle+1) None
      | Some (i, value) ->
        make_cycle insts (cycle+1) (Some (i-1, value))
      | None ->
        match insts with
        | Noop :: insts' -> make_cycle insts' (cycle+1) None
        | Add(n):: insts' -> make_cycle insts' (cycle+1) (Some (0, n))
        | [] -> ()

    in
    make_cycle insts 1 None


end

let () =
  print_endline "";
  let insts = List.map parse_line lines
  and test_insts = List.map parse_line test_lines
  in
  Printf.printf "\nPART1: %d\n" (Part1.execute insts);

  let insts = List.map parse_line lines in
  print_endline "PART2"; Part2.execute insts;

