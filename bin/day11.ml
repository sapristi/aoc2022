let file = "./inputs/day11" 

let lines = Aoc.read_lines file
let test_lines = Aoc.read_lines "./inputs/day11_test"


module Operation = struct
  type operand =
    | Const of int
    | Old
  [@@deriving show]
  type operator = | Plus | Times
  [@@deriving show]

  type t = (operand * operator * operand)
  [@@deriving show]

  let parse_operand token =
    if token = "old" then Old else Const (int_of_string token)

 let parse_operator token =
   if token = "*" then Times
   else if token = "+" then Plus
   else failwith "Bad token"

 let parse_line line = 
   if not (String.starts_with "  Operation: new = " line) then failwith "Bad line";
   Printf.printf "OPERATION %s -> %s\n" line (CCString.drop 19 line);
   let [token1; token2; token3 ] = CCString.drop 19 line |> String.split_on_char ' ' in 
   (parse_operand token1, parse_operator token2, parse_operand token3)


 let get_operand_value operand level =
   match operand with
   | Const (n) -> n
   | Old -> level

 let compute (operation: t) level =
   match operation with
   | (token1, Plus, token2) -> (get_operand_value token1 level) + (get_operand_value token2 level)
   | (token1, Times, token2) -> (get_operand_value token1 level) * (get_operand_value token2 level)
end


module Monkey = struct

  type t = {
    id: int;
    items: int CCDeque.t;
    operation: Operation.t;
    test: int;
    iftrue: int;
    iffalse: int;
    mutable inspected_count: int;
  }
  [@@deriving show]
  let parse_starting_items line =
    Printf.printf "STARTING %s -> %s\n" line (CCString.drop 18 line);
    let items_s = CCString.drop 18 line in
    String.split_on_char ',' items_s |> List.map String.trim |> List.map int_of_string |> CCDeque.of_list


  let rec parse_lines lines result current_id =
    match lines with
    | [] -> result
    | "" :: lines' -> parse_lines lines' result current_id
    | monkey_id_l :: starting_items_l :: operation_l :: test_l :: iftrue_l :: iffalse_l :: lines' ->
      let new_monkey = {
        id=current_id;
        items=parse_starting_items starting_items_l;
        operation=Operation.parse_line operation_l;
        test=CCString.drop 21 test_l |> int_of_string;
        iftrue=CCString.drop 29 iftrue_l |> int_of_string;
        iffalse=CCString.drop 30 iffalse_l |> int_of_string;
        inspected_count=0;
      } in
      parse_lines lines' (new_monkey::result) (current_id+1)

  type inspect_result = {
    target: int; item: int;
  }

  let inspect monkey relief_operation =
    monkey.inspected_count <- monkey.inspected_count + 1;

    let item_level = CCDeque.take_front monkey.items in
    let op_level = Operation.compute monkey.operation item_level in
    let relief_level = relief_operation op_level in
    if relief_level mod monkey.test = 0
    then {target=monkey.iftrue; item=relief_level} else {target=monkey.iffalse; item=relief_level}


  let make_round monkeys relief_operation =
    for i=0 to Array.length monkeys -1 do
      let m = monkeys.(i) in
      while CCDeque.length m.items > 0 do
        let {target; item} = inspect m relief_operation in
        Printf.printf "M %d: [%d] -> %d\n" i item target;
        CCDeque.push_back monkeys.(target).items item;
      done
    done
end




let part1 () =
  print_endline "_____";
  let monkeys = Monkey.parse_lines lines [] 0 |> List.rev |> Array.of_list in
  Array.iter (fun m -> Printf.printf "%s\n" (Monkey.show m)) monkeys;


  for i = 0 to 19 do
    Monkey.make_round monkeys (fun x -> x / 3)
  done;

  let counts = Array.map (fun (m: Monkey.t) -> m.inspected_count) monkeys in 
  Array.sort compare counts;
  Array.iter (fun x -> Printf.printf " %d;" x) counts;
  print_endline "\n"



let part2 () =
  print_endline "_____";
  let monkeys = Monkey.parse_lines lines [] 0 |> List.rev |> Array.of_list in
  Array.iter (fun m -> Printf.printf "%s\n" (Monkey.show m)) monkeys;


  let test_values = Array.map (fun (m:Monkey.t) ->m.test) monkeys in
  let tests_mod = Array.fold_left (fun x y -> x * y) 1 test_values in

  let print_levels monkeys =  
    let counts = Array.map (fun (m: Monkey.t) -> m.inspected_count) monkeys in 
    Array.sort compare counts;
    Array.iter (fun x -> Printf.printf " %d;" x) counts;
    print_endline "\n"
  in 

  for i = 0 to 9999 do
    Monkey.make_round monkeys (fun x -> x mod tests_mod)
  done;

  print_levels monkeys


let () = part2 ()
