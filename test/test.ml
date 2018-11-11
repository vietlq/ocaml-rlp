open OUnit
module Rlp = Rlp.Rlp

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

let suite =
  "Rlp" >::: [
    "rlp_encode_char_0" >:: rlp_encode_char_0;
    "rlp_encode_char_1" >:: rlp_encode_char_1;
    "rlp_encode_char_127" >:: rlp_encode_char_127;
    "rlp_encode_char_128" >:: rlp_encode_char_128;
    "rlp_encode_char_129" >:: rlp_encode_char_129;
    "rlp_encode_char_255" >:: rlp_encode_char_255;
  ]

let _ = run_test_tt_main suite
