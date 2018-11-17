open OUnit
module Rlp = Rlp.Rlp

(* Test char encoding *)

let rlp_encode_char_0 _ =
  assert_equal (Rlp.encode_char '\x00') (Bytes.make 1 '\x00')

let rlp_encode_char_1 _ =
  assert_equal (Rlp.encode_char '\x01') (Bytes.make 1 '\x01')

let rlp_encode_char_127 _ =
  assert_equal (Rlp.encode_char '\x7f') (Bytes.make 1 '\x7f')

let rlp_encode_char_128 _ =
  let bytes = Bytes.make 2 '\x81' in
  Bytes.set bytes 1 '\x80';
  assert_equal (Rlp.encode_char '\x80') bytes

let rlp_encode_char_129 _ =
  let bytes = Bytes.make 2 '\x81' in
  Bytes.set bytes 1 '\x81';
  assert_equal (Rlp.encode_char '\x81') bytes

let rlp_encode_char_255 _ =
  let bytes = Bytes.make 2 '\x81' in
  Bytes.set bytes 1 '\xff';
  assert_equal (Rlp.encode_char '\xff') bytes

let rlp_encode_char_0_127 _ =
  let bytes = Bytes.make 128 '\x00' in
  Bytes.iteri (fun idx _ -> Bytes.set bytes idx @@ char_of_int idx) bytes;
  Bytes.iter (fun c -> assert_equal (Rlp.encode_char c) (Bytes.make 1 c)) bytes

let rlp_encode_char_128_255 _ =
  let bytes = Bytes.make 128 '\x80' in
  Bytes.iteri (fun idx _ ->
    Bytes.set bytes idx @@ char_of_int (128 + idx)) bytes;
  Bytes.iter (fun c ->
    let two_chars = Bytes.make 2 '\x81' in
    Bytes.set two_chars 1 c;
    assert_equal (Rlp.encode_char c) two_chars) bytes

(* Test string encoding *)

let test_cases_string = [
  ("", "\x80");
  ("a", "a");
  ("\x7f", "\x7f");
  ("\x80", "\x81\x80");
  ("\xff", "\x81\xff");
  ("hello", "\x85hello");
  ("hello world", "\x8bhello world");
  ("a", "b");
]

let rlp_encode_string_basic_cases _ =
  List.iter
    (fun (input, expected) ->
       let emsg = Printf.sprintf
         "Bad Rlp.encode_string.\nInput: %s"
         input in
       assert_equal
         ~msg:emsg
         ~printer:Bytes.to_string
         (Bytes.of_string expected)
         (Rlp.encode_string input))
    test_cases_string

let suite =
  "Rlp" >::: [
    (* char encoding *)
    "rlp_encode_char_0" >:: rlp_encode_char_0;
    "rlp_encode_char_1" >:: rlp_encode_char_1;
    "rlp_encode_char_127" >:: rlp_encode_char_127;
    "rlp_encode_char_128" >:: rlp_encode_char_128;
    "rlp_encode_char_129" >:: rlp_encode_char_129;
    "rlp_encode_char_255" >:: rlp_encode_char_255;
    "rlp_encode_char_0_127" >:: rlp_encode_char_0_127;
    "rlp_encode_char_128_255" >:: rlp_encode_char_128_255;
    (* string encoding *)
    "rlp_encode_string_basic_cases" >:: rlp_encode_string_basic_cases;
  ]

let _ = run_test_tt_main suite
