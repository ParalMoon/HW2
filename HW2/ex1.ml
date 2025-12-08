(* gcd *)

let rec gcd n m =
  if n = 0 && m = 0 then 0
  else if n < m then gcd m n
  else if m = 0 then n
  else gcd (n - m) m

let () =
  print_endline (string_of_int (gcd 10 0));
  print_endline (string_of_int (gcd 9 5));
  print_endline (string_of_int (gcd 13 13));
  print_endline (string_of_int (gcd 37 600));
  print_endline (string_of_int (gcd 0 0));
