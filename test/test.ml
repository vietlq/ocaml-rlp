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

let str_55_a = Bytes.make 55 'a'
let str_55_a_rlp =
  let prefix = Bytes.make 1 '\xb7' in
  Bytes.cat prefix str_55_a

let str_56_a = Bytes.make 56 'a'
let str_56_a_rlp =
  let prefix = Bytes.make 2 '\xb8' in
  Bytes.set prefix 1 (char_of_int 56);
  Bytes.cat prefix str_56_a

let str_255_a = Bytes.make 255 'a'
let str_255_a_rlp =
  let prefix = Bytes.make 2 '\xb8' in
  Bytes.set prefix 1 (char_of_int 255);
  Bytes.cat prefix str_255_a

let str_256_a = Bytes.make 256 'a'
let str_256_a_rlp =
  let prefix = Bytes.make 3 '\xb9' in
  Bytes.set prefix 1 (char_of_int 1);
  Bytes.set prefix 2 (char_of_int 0);
  Bytes.cat prefix str_256_a

let str_65536_a = Bytes.make 65536 'a'
let str_65536_a_rlp =
  let prefix = Bytes.make 4 '\xba' in
  Bytes.set prefix 1 (char_of_int 1);
  Bytes.set prefix 2 (char_of_int 0);
  Bytes.set prefix 3 (char_of_int 0);
  Bytes.cat prefix str_65536_a

let test_cases_string = [
  ("", "\x80");
  ("a", "a");
  ("\x7f", "\x7f");
  ("\x80", "\x81\x80");
  ("\xff", "\x81\xff");
  ("hello", "\x85hello");
  ("hello world", "\x8bhello world");
  (Bytes.to_string str_55_a, Bytes.to_string str_55_a_rlp);
  (Bytes.to_string str_56_a, Bytes.to_string str_56_a_rlp);
  (Bytes.to_string str_255_a, Bytes.to_string str_255_a_rlp);
  (Bytes.to_string str_256_a, Bytes.to_string str_256_a_rlp);
  (Bytes.to_string str_65536_a, Bytes.to_string str_65536_a_rlp);
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

let rlp_decode_string_basic_cases _ =
  List.iter
    (fun (expected, input) ->
       let emsg = Printf.sprintf
         "Bad Rlp.decode.\nInput: %s"
         input in
       assert_equal
         ~msg:emsg
         ~printer:(fun x -> match x with
          | (Some (Rlp.RlpData s)) -> s
          | _ -> failwith "Impossible!")
         (Some (Rlp.RlpData expected))
         (Rlp.decode (Bytes.of_string input)))
    test_cases_string

let test_cases_int = [
  ("", 0);
  ("a", int_of_char 'a');
  ("\x7f", 127);
  ("\x80", 128);
  ("\xff", 255);
  ("\x01\x00", 256);
  ("\x01\x00\x00", 65536);
  ("\x01\x00\x01", 65537);
]

let rlp_decode_int_basic_cases _ =
  List.iter
    (fun (input, expected) ->
       let emsg = Printf.sprintf
         "Bad Rlp.decode_small_int_string.\nInput: %s"
         input in
       assert_equal
         ~msg:emsg
         ~printer:(string_of_int)
         (expected)
         (Rlp.decode_small_int_string input))
    test_cases_int

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
    (* string decoding *)
    "rlp_decode_string_basic_cases" >:: rlp_decode_string_basic_cases;
    (* int decoding *)
    "rlp_decode_int_basic_cases" >:: rlp_decode_int_basic_cases;
  ]

let _ = run_test_tt_main suite
