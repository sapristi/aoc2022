let file = "./inputs/day5"

let lines = Aoc.read_lines file

let test_lines = [
  "    [D]    ";
  "[N] [C]    ";
  "[Z] [M] [P]";
  " 1   2   3 ";
  "";
  "move 1 from 2 to 1";
  "move 3 from 1 to 3";
  "move 2 from 2 to 1";
  "move 1 from 1 to 2";
]

module Action = struct
  type t = {
    qtt: int;
    from: int;
    dest: int;
  }

  let format {qtt; from; dest} = Printf.sprintf "%d [%d -> %d]" qtt from dest

  let parse line =
    let [_; qtt_s; _; from_s; _; dest_s ] = String.split_on_char ' ' line in
    let res = {
      qtt = (int_of_string qtt_s);
      from = (int_of_string from_s)-1;
      dest = (int_of_string dest_s)-1;
    } in
    Printf.printf "%s --> [%s]\n" line (format res);
    res

end

module Stacks = struct
  type t = char list array

  let add (stacks:t) (i:int) (c:char) =
    stacks.(i) <- c::stacks.(i)

  let pop (stacks:t) (i:int) =
    match stacks.(i) with
    | [] -> failwith "Can't pop!"
    | h::t ->stacks.(i) <- t; h

  let move (stacks:t) (from:int) (dest:int) =
    add stacks dest (pop stacks from)


  let format (stacks:t) : string =
    let format_elem list i =
      if i < List.length list
      then Printf.sprintf "[%c]" (List.nth list i)
      else "   "
    in
    let res = ref "" in
    for i = 0 to (Array.fold_left Int.max 0 (Array.map List.length stacks)) -1 do
      res := (!res) ^ (
          Array.map (fun stack -> format_elem stack i) stacks
          |> Array.to_list
          |> String.concat " "
        ) ^ "\n";
    done;
    !res

  let apply_action stacks ({qtt; from; dest}: Action.t) =
    (* Printf.printf "Stacks:\n%s\n Action: %s\n" (format stacks) (Action.format {qtt;from;dest}); *)
    for i = 0 to qtt -1 do
      move stacks from dest
    done

  let take stacks from qtt =
    let rec take_rec list n res =
      if n = 0 then (res, list) else
        let h::t = list in take_rec t (n-1) (h::res)
    in let taken, remainder = take_rec stacks.(from) qtt [] in
    stacks.(from) <- remainder;
    List.rev taken

  let apply_action_p2 stacks ({qtt; from; dest}: Action.t) =

    Printf.printf "Stacks:\n%s\n Action: %s\n" (format stacks) (Action.format {qtt;from;dest});
    let moved = take stacks from qtt in
    stacks.(dest) <- List.concat [moved; stacks.(dest)]

end

let parse_crate_line (stacks:Stacks.t)  nb_stacks line=
 for i = 0 to nb_stacks -1 do
    let char = line.[1+i*4] in
    if char != ' ' then Stacks.add stacks i char
  done

let parse_lines lines nb_stacks =
  let rec split_lines_groups lines crate_lines =
    match lines with
    | [] -> failwith "problem"
    | line::t ->
      if String.contains line '1' then (crate_lines, List.tl t) else split_lines_groups t (line::crate_lines)
  in
  let (crate_lines, action_lines) = split_lines_groups lines [] in
  let stacks = Array.init nb_stacks (fun _ -> []) in
  List.iter (parse_crate_line stacks nb_stacks) (crate_lines);

  Printf.printf "%s" (Stacks.format stacks);

  (stacks, List.map Action.parse action_lines)

let () =
  print_endline "";
  (* let (stacks, actions) = parse_lines test_lines 3 in *)
  let (stacks, actions) = parse_lines lines 9 in

  List.iter (Stacks.apply_action stacks) actions;
  Printf.printf "Part 1\n%s\n" (Stacks.format stacks);

  (* let (stacks, actions) = parse_lines test_lines 3 in *)
  let (stacks, actions) = parse_lines lines 9 in
  List.iter (Stacks.apply_action_p2 stacks) actions;
  Printf.printf "Part 2\n%s\n" (Stacks.format stacks);
