let file = "./inputs/meili"

let lines = Aoc.read_lines file

let test_lines = [
    "tamo - RLRLR";
    "loic - RLLL";
    "kero - LRLR";
    "luna - LRRR";
    "caro - LRL";
    "lena - RLLR";
    "thomas - LRLL";
    "tommy - LLL";
    "chayaline - LRLL";
]

type tag = | L | R

let char_to_tag c = match c with | 'L' -> L | 'R' -> R | _ -> failwith "wrong char"
let parse_insts insts =
  String.fold_right (fun char res -> (char_to_tag char) :: res) insts []

let format_tag_list tags =
  String.concat "" (List.map (fun tag -> match tag with |L -> "L"|R-> "R") tags) 

let parse_line line =
  let [name_; insts_ ] = String.split_on_char '-' line in
  let name = String.sub name_ 0 ((String.length name_) -1) and
    insts = String.sub insts_ 1 ((String.length insts_) -1) in
  (name, parse_insts insts)



module Part1 = struct
  type tree =
    | T of (tree * tree * string list)
    | End of string list

  let rec simple_tree name insts =
    match insts with
    | [] -> End([name])
    | L::insts_t -> T(simple_tree name insts_t, End([]), [])
    | R::insts_t -> T(End([]), simple_tree name insts_t, [])

  let rec add_to_tree name insts tree =
    match insts, tree with
    | [], End(names) -> End(name::names)
    | [], T(lt, rt, names) -> T(lt, rt, name::names)
    | L::insts_t, End(names) ->
      T(simple_tree name insts_t, End([]), names)
    | R::insts_t, End(names) ->
      T(End([]), simple_tree name insts_t, names)
    | L::insts_t, T(lt, rt, names) ->
      T(add_to_tree name insts_t lt, rt, names)
    | R::insts_t, T(lt, rt, names) ->
      T(lt, add_to_tree name insts_t rt, names)

  let add_parsed_line_to_tree tree parsed_line =
    let (name, insts) = parsed_line in
    add_to_tree name insts tree

  let add_names names depth res = 
    List.concat [
      List.map (fun name -> (depth, name)) names;
      res
    ]

  let rec walk t current_depth (res: (int*string) list) =
    match t with
    | End(names) -> add_names names current_depth res
    | T(t_l, End([]), names) ->
      walk t_l current_depth (
        add_names names current_depth res
      )
    | T(End([]), t_r, names) ->
      walk t_r current_depth (
        add_names names current_depth res
      )
    | T(t_l, t_r, names) ->
      walk t_l (current_depth+1) (
        walk t_r (current_depth +1) (add_names names current_depth res)
      )

  let min_value elems =
    List.fold_left
      (fun res (d, _) -> Int.min d res)
      Int.max_int
      elems

  let print_elem (d, name) = Printf.printf "%d -> %s\n" d name

  let rec find_first_min elems min =
    match elems with
    | (d, name)::t ->
      if d == min then (d, name)
      else find_first_min t min
    | [] -> failwith "empty"

  let tree_from_lines lines =
    let parsed_lines = List.map parse_line lines in
    List.fold_left
        add_parsed_line_to_tree
        (End([]))
        parsed_lines

  let find_collapsed_names t =
    let collapsed_names = ref [] in
    let add_names names current_path_rev =
      let current_path = List.rev current_path_rev in
      collapsed_names := List.concat [
          List.map (fun name -> (name, current_path)) names;
          !collapsed_names 
        ]

    in
    let rec find_collapsed_names_rec t current_path=
      match t with
      | End(names) -> add_names names current_path
      | T(t_l, End([]), []) -> find_collapsed_names_rec t_l current_path
      | T(End([]), t_r, []) -> find_collapsed_names_rec t_r current_path
      | T(t_l, t_r, names) -> (
          add_names names current_path;
          find_collapsed_names_rec t_l (L::current_path);
          find_collapsed_names_rec t_r (R::current_path);
        )
    in
    find_collapsed_names_rec t [];
    !collapsed_names

  let rec distance p1 p2 =
    match p1, p2 with
      | L::t1, L::t2 -> distance t1 t2
      | R::t1, R::t2 -> distance t1 t2
      | _ -> (List.length p1) + (List.length p2)


  let find_min seq comp =
    (* let format_item (name, (tags, i)) = Printf.sprintf "%s at %s (%d)" name (format_tag_list tags) i in *)
    let rec find_min_seq (seq: 'a Seq.t) comp start =
      Seq.fold_left
        (fun res item ->
           (* Printf.printf "Compare %s %s -->" (format_item item) (format_item res); *)
           if comp res item < 0
           then res (* (Printf.printf "Kept %s\n" (format_item res); res) *)
           else item  (* (Printf.printf "Kept %s\n" (format_item item); item) *)
        )
        start seq
    in
    match Seq.uncons seq with
    | None -> failwith "empty seq"
    | Some (elem, seq') -> find_min_seq seq' comp elem

  (* let rec find_min list comp = *)
  (*   List.fold_left (fun res item -> if comp res item < 0 then item else res) list *)

  let walk_p2 (collapsed_names: (string * tag list)  list) =
    let table = Hashtbl.create (List.length collapsed_names) in
    let dists = Hashtbl.create ((List.length collapsed_names)*(List.length collapsed_names)) in
    List.iteri (fun i (name, path) -> Hashtbl.add table name (path, i)) collapsed_names;
    (* List.iter *)
    (*   (fun (name1, path1) (name2, path2) -> Hashtbl.add dists (name1, name2) (distance path1 path2)) *)
    (*   (("root", [])::collapsed_names) (("root", [])::collapsed_names); *)

    List.iter
      (fun (name1, path1)  ->
         List.iter (fun (name2, path2) -> Hashtbl.add dists (name1, name2) (distance path1 path2))
           (("root", [])::collapsed_names))
      (("root", [])::collapsed_names);


    (* print_string "\nTable:\n"; *)
    (* Hashtbl.iter (fun name (tags, i) -> Printf.printf "%s at %s (%d)\n" name (format_tag_list tags) i)table; *)

    (* print_string "\nDists\n"; *)
    (* Hashtbl.iter (fun (name1, name2)  (d) -> Printf.printf "%s - %s =%d\n" name1 name2 d) dists; *)
    (* print_string "\n"; *)

    let dists_sum = ref 0 in
    let current_path = ref [] and current_name = ref "root" in
    while Hashtbl.length table > 0 do
      let (next_name, (next_path, _)) = find_min
          (Hashtbl.to_seq table)
          (fun (name1, (_, i1)) (name2, (_, i2)) ->
                compare
                  ((Hashtbl.find dists (name1, !current_name)), i1)
                  ((Hashtbl.find dists (name2, !current_name)), i2)
          )
      in
      let to_add = Hashtbl.find dists (!current_name, next_name) in
      Printf.printf "Found %s at %s (%d)\n" next_name (format_tag_list next_path) to_add;
      dists_sum := (!dists_sum) + to_add;
      current_path := next_path;
      current_name := next_name;
      Hashtbl.remove table next_name;
    done;
    !dists_sum

  let rec compare_tags tl1 tl2 =
    match tl1,tl2 with
    | [], [] -> 0
    | h::_, [] -> if h = L then -1 else 1
    | [],  h::_-> if h = L then 1 else -1
    | h1::t1, h2::t2  -> if h1 = h2 then compare_tags t1 t2 else compare h1 h2

  let solution lines =
    let parsed_lines = List.map parse_line lines in
    let t = List.fold_left
        add_parsed_line_to_tree
        (End([]))
        parsed_lines in
    let elems = walk t 0 []
    in
    (* List.iter (fun (d, name) -> Printf.printf "%d -> %s\n" d name) res; *)
    let min_depth = min_value elems in
    let min_elem = find_first_min elems min_depth
    in print_string "\nPart1: ";
    print_elem min_elem;

   
    let collapsed_names = find_collapsed_names t in
    let sorted_collapsed_names = (List.sort (fun (_,p1) (_, p2) -> compare p1 p2) collapsed_names) in
    (* List.iter (fun (name, path) -> Printf.printf "%s -> %s\n" name (format_tag_list path)) collapsed_names; *)
    (* List.iter *)
    (*   (fun (name, path) -> Printf.printf "%s -> %s\n" name (format_tag_list path)) *)
    (*   (List.sort (fun (_,p1) (_, p2) -> compare_tags p1 p2) collapsed_names); *)
    let res = walk_p2 sorted_collapsed_names in
    Printf.printf "\nRES P2 %d\n" res;
end

module Elem = struct

  let min elems =
    List.fold_left
      (fun res (d, _) -> Int.min d res)
      Int.max_int
      elems

  let print (d, name) = Printf.printf "%d -> %s\n" d name

  let rec find_first_min elems min =
    match elems with
    | (d, name)::t ->
      if d == min then (d, name)
      else find_first_min t min
    | [] -> failwith "empty"

  let rec all_mins elems current_min res =
    match elems with
    | [] -> res
    | (d, name)::t ->
      if d < current_min then
        all_mins t d [(d, name)]
      else if d == current_min then
        all_mins t d ((d,name)::res)
      else all_mins t current_min res
end

let () =
  Part1.solution lines;
  (* Part2.solution lines *)
