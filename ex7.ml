(*fold3*)

let rec fold3 f acc l1 l2 l3 =
  match (l1, l2, l3) with
  | ([], [], []) -> acc
  | (x1::xs1, x2::xs2, x3::xs3) ->
      fold3 f (f acc x1 x2 x3) xs1 xs2 xs3
  | _ -> failwith "Lists must have same length"

let () =
  let result1 =
    fold3 (fun a b c d -> a + b + c + d)
      10 [33;67;12;33] [10;23;84;57] [11;55;23;58]
  in
  print_endline (string_of_int result1);

  let result2 =
    fold3 (fun a b c d -> (-a) + b + c + d)
      4 [11;63;-45;22] [75;123;-44;1] [55;24;20;3]
  in
  print_endline (string_of_int result2);

  let result3 =
    fold3 (fun a b c d -> a * b * c * d)
      55 [] [] []
  in
  print_endline (string_of_int result3);

  let result4 =
    fold3 (fun a b c d -> (a * b * c + d) mod 7)
      33 [12;33] [10;7] [5;12]
  in
  print_endline (string_of_int result4);

  let result5 =
    fold3 (fun a b c d -> if b then a + c else a + d)
      34 [true;false;false;true] [12;3;4;77] [11;23;6;100]
  in
  print_endline (string_of_int result5);

  let result6 =
    fold3 (fun a b c d -> if b then a else c + d)
      55 [true;true;false;false;true]
         [111;63;88;123;98]
         [0;23;778;34;6]
  in
  print_endline (string_of_int result6);
