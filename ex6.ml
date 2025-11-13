(* sigma *)
let sigma a b f =
  let rec sum n acc =
    if n > b then acc
    else sum (n + 1) (acc + f n)
  in
  if a > b then 0
  else sum a 0

let () =
  print_endline (string_of_int (sigma 10 10 (fun x -> x)));
  print_endline (string_of_int (sigma 11 10 (fun x -> x)));
  print_endline (string_of_int (sigma 10 5 (fun x -> x)));
  print_endline (string_of_int (sigma 1 10 (fun x -> if x mod 2 = 0 then 1 else 0)));
  print_endline (string_of_int (sigma 2 10 (fun x -> x + 10)));
  print_endline (string_of_int (sigma 0 100 (fun x -> 0)));
  print_endline (string_of_int (sigma 10 12 (fun x -> 2 * x)))