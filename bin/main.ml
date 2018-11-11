open Rlp
module Math = Rlp__Math

let () =
  print_endline "Hello, World! See the math:";
  print_int @@ Math.add 2 3;
  print_endline "\nRlp:";
  print_string @@ Bytes.to_string @@ Rlp.encode_int 123;
  print_endline "\nNice try!";
