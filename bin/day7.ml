let file = "./inputs/day7"

let lines = Aoc.read_lines file

let test_lines = [
  "$ cd /";
  "$ ls";
  "dir a";
  "14848514 b.txt";
  "8504156 c.dat";
  "dir d";
  "$ cd a";
  "$ ls";
  "dir e";
  "29116 f";
  "2557 g";
  "62596 h.lst";
  "$ cd e";
  "$ ls";
  "584 i";
  "$ cd ..";
  "$ cd ..";
  "$ cd d";
  "$ ls";
  "4060174 j";
  "8033020 d.log";
  "5626152 d.ext";
  "7214296 k";
]


module Part1 = struct

  type directory =
    {
      name: string;
      files: (string * int) list;
      children: directory list;
    }

  let files_sizes dir =
    List.fold_left (fun res (_, size) -> size+res) 0 dir.files 

  let sum items = List.fold_left (+) 0 items

  type term_line =
    | CD_root
    | CD_up
    | CD of string
    | LS
    | Dir of string
    | File of (string*int)

  let parse_line l =
    if l = "$ cd .." then CD_up
    else if l = "$ cd /" then CD_root
    else if l = "$ ls" then LS
    else if String.starts_with "$ cd" l
    then CD (String.sub l 5 ((String.length l) - 5))
    else if String.starts_with "dir" l
    then Dir (String.sub l 4 ((String.length l) - 4))
    else let [size_s; name] = String.split_on_char ' ' l in
      File (name, int_of_string size_s)


  let format_path path =
    String.concat "" [
      "[ ";
      List.map (fun name -> Printf.sprintf "'%s'; " name) (List.rev path) |> String.concat "";
      "]";
    ]

  let get_sizes lines =
    let table = Hashtbl.create (List.length lines) in
    let rec add_file size path =
      let name = path |> List.rev |> String.concat "/" in
      let current_size = match Hashtbl.find_opt table name with | None -> 0 | Some(size) -> size
      in Hashtbl.replace table name (size+current_size);
      (* Printf.printf "%s: %d -> %d\n" (format_path path) current_size (size+current_size) ; *)
      match path with
      | [] -> ()
      | _::path' -> add_file size path'

    in
    let rec add_lines lines current_path =
      match lines with
      | [] -> ()
      | l::lines' ->
        match parse_line l with
        | CD_root -> add_lines lines' []
        | CD_up -> (* print_endline "CD .."; *) add_lines lines' (List.tl current_path)
        | CD(dirname) -> (* Printf.printf "CD %s\n" dirname; *) add_lines lines' (dirname::current_path)
        | LS -> add_lines lines' current_path
        | Dir(_) -> add_lines lines' current_path
        | File(name, size) ->
          (* Printf.printf "add %s (%d) to %s\n" name size (format_path current_path); *)
          add_file size current_path;
          add_lines lines' current_path
    in
    add_lines lines [];
    Hashtbl.to_seq table |> List.of_seq

  let solution1 lines =
    let table_values = get_sizes lines in
    let res = List.fold_left (fun res (name, size) -> if size <= 100000 then res+size else res) 0 table_values in
    Printf.printf "\nPART1: %d\n" res

  let solution2 lines =
    let table_values = get_sizes lines |> List.sort compare in
    (* let to_print = List.fold_left (fun res (name, size) -> if size >= 8381165 then (name, size)::res else res) [] table_values in *)
    let (_, occupied_space) = List.hd table_values in
    let total_size = 70000000 and needed = 30000000 in
    let free_space = total_size - occupied_space in
    let to_remove = needed - free_space in
    List.iter (fun (name, size) -> Printf.printf "%s -> %d\n" name size ) table_values;
    let res = List.fold_left (fun res (_, size) -> if size >= to_remove && size <= res then size else res) Int.max_int table_values in
    Printf.printf "\nPART2: occupied %d\nto_free %d\n found %d\n"  occupied_space to_remove  res

end


let () =
  print_endline "";
  (* Part1.solution1 lines; *)
  Part1.solution2 lines;
  
