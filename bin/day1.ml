let file = "./inputs/day1";;

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


let rec parse_input current res lines =
  match lines with
      | [] -> res
      | h::t -> if String.length h = 0
                   then parse_input 0 (current::res) t
                   else (
                     let v = int_of_string h in
                     parse_input (v+current) res t
                   )



let first_part () =
  let lines = read_lines file in
  let input = parse_input 0 [] lines in
  let res = List.fold_left Int.max 0 input in
  print_int res



type top_three = {
  mutable first: int;
  mutable second: int;
  mutable third: int;
}

let update_top_three tt new_val =
  if new_val > tt.first
  then {
    first= new_val;
    second= tt.first;
    third= tt.second
  }
  else if new_val > tt.second
  then {
    first= tt.first;
    second= new_val;
    third= tt.second
  }
  else if new_val > tt.third
  then
    {
      first= tt.first;
      second= tt.second;
      third= new_val
    }
  else tt

let extract_top_three_rec tt values =
  List.fold_left update_top_three tt values

let extract_top_three values =
  match values with
  | h1::h2::h3::t ->
    let hh1::hh2::hh3::[] = List.sort (-) [h1;h2;h3] in
    List.fold_left update_top_three {first=hh3; second=hh2; third=hh1} t
  | _ -> failwith "Not enough elems"

let second_part () =
  let lines = read_lines file in
  let input = parse_input 0 [] lines in
  let res = extract_top_three input in
  print_string (Printf.sprintf "%d - %d - %d\n" res.first res.second res.third);
  print_int (res.third + res.second + res.first)
 

let () =
  second_part ()
