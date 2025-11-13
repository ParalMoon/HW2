(* goldbach *)
let is_prime n =
  if n < 2 then false
  else if n = 2 then true
  else if n mod 2 = 0 then false
  else
    let rec check_divisor d =
      if d * d > n then true
      else if n mod d = 0 then false
      else check_divisor (d + 2)
    in
    check_divisor 3

let goldbach n =
  let rec find_pair p =
    if p > n / 2 then None
    else if is_prime p && is_prime (n - p) then Some (p, n - p)
    else find_pair (p + 1)
  in
  find_pair 2

let goldbach_list_limit lower upper limit =
  let rec collect n acc =
    if n > upper then List.rev acc
    else if n mod 2 = 0 && n > 2 then
      match goldbach n with
      | Some (p1, p2) when p1 >= limit ->
          collect (n + 1) ((n, (p1, p2)) :: acc)
      | _ -> collect (n + 1) acc
    else
      collect (n + 1) acc
  in
  collect lower []

let print_goldbach_list lst =
  print_string "[";
  List.iteri (fun i (n, (p1, p2)) ->
    if i > 0 then print_string "; ";
    Printf.printf "(%d, (%d, %d))" n p1 p2
  ) lst;
  print_endline "]"

let () =
  print_goldbach_list (goldbach_list_limit 9 20 5);
  print_goldbach_list (goldbach_list_limit 25 70 10);
  print_goldbach_list (goldbach_list_limit 100 100 100);
  print_goldbach_list (goldbach_list_limit 100 200 19);
  print_goldbach_list (goldbach_list_limit 50 500 20);
  print_goldbach_list (goldbach_list_limit 1 2000 50)