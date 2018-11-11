open OUnit
module Rlp = Rlp.Rlp

let rlp_encode_char_0 _ =
  assert_equal (Rlp.encode_char '\x00') (Bytes.make 1 '\x00')

let rlp_encode_char_1 _ =
  assert_equal (Rlp.encode_char '\x01') (Bytes.make 1 '\x01')

let rlp_encode_char_127 _ =
  assert_equal (Rlp.encode_char '\x7f') (Bytes.make 1 '\x7f')

let suite =
  "Rlp" >::: [
    "rlp_encode_char_0" >:: rlp_encode_char_0;
    "rlp_encode_char_1" >:: rlp_encode_char_1;
    "rlp_encode_char_127" >:: rlp_encode_char_127;
  ]

let _ = run_test_tt_main suite
