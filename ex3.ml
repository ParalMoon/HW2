(* factor_list *)

let factor_list n =
  let rec count_factor num divisor count =
    if num mod divisor = 0 then
      count_factor (num / divisor) divisor (count + 1)
    else
      (num, count)
  in
  let rec find_factors num divisor acc =
    if divisor * divisor > num then
      if num > 1 then acc @ [(num, 1)] else acc
    else if num mod divisor = 0 then
      let (remaining, count) = count_factor num divisor 0 in
      find_factors remaining (divisor + 1) (acc @ [(divisor, count)])
    else
      find_factors num (divisor + 1) acc
  in
  find_factors n 2 []

let print_factor_list lst =
  print_string "[";
  List.iteri (fun i (factor, count) ->
    if i > 0 then print_string "; ";
    Printf.printf "(%d, %d)" factor count
  ) lst;
  print_endline "]"

let () =
  print_factor_list (factor_list 10);
  print_factor_list (factor_list 17);
  print_factor_list (factor_list 27);
  print_factor_list (factor_list 315);
  print_factor_list (factor_list 777);
  print_factor_list (factor_list 1024)