(*palindrome*)

let palindrome lst =
  lst = List.rev lst

let () =
  print_endline (string_of_bool (palindrome ["1"; "2"; "3"; "4"]));
  print_endline (string_of_bool (palindrome ["x"; "m"; "a"; "s"]));
  print_endline (string_of_bool (palindrome ["a"; "m"; "o"; "r"; "e"; "r"; "o"; "m"; "a"]));
  print_endline (string_of_bool (palindrome ["1"; "2"; "3"; "2"; "1"]));
  print_endline (string_of_bool (palindrome ["b"; "o"; "r"; "r"; "o"; "w"; "o"; "r"; "r"; "o"; "b"]))