(* gcd *)

let rec gcd n m =
  (* Case: both zero -> return 0 by definition in test cases *)
  if n = 0 && m = 0 then 0
  (* Ensure n >= m 조건을 만족시키기 위해 swap *)
  else if n < m then gcd m n
  (* Euclid base case *)
  else if m = 0 then n
  (* Recursive subtraction *)
  else gcd (n - m) m

(* simple test printing *)
let () =
  print_endline (string_of_int (gcd 10 0));
  print_endline (string_of_int (gcd 9 5));
  print_endline (string_of_int (gcd 13 13));
  print_endline (string_of_int (gcd 37 600));
  print_endline (string_of_int (gcd 0 0));
