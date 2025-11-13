(* phi *)
let rec gcd n m =
  if m = 0 then n else gcd m (n mod m)

let phi m =
  if m = 1 then 1
  else
    let rec count_coprime r acc =
      if r >= m then acc
      else if gcd r m = 1 then count_coprime (r + 1) (acc + 1)
      else count_coprime (r + 1) acc
    in
    count_coprime 1 0

let () =
  print_endline (string_of_int (phi 4));
  print_endline (string_of_int (phi 9));
  print_endline (string_of_int (phi 10));
  print_endline (string_of_int (phi 17));
  print_endline (string_of_int (phi 30))