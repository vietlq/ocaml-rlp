module Rlp :
  sig
    type t = RlpData of string | RlpList of t list
    val encode_non_value : bytes
    val encode_empty_list : bytes
    val encode_bool : bool -> bytes
    val encode_char : char -> bytes
    val big_endian_bytes_of_uint : int -> bytes
    val encode_int : int -> bytes
    val encode_bytes : bytes -> bytes
    val encode_string : string -> bytes
    val encode : t -> bytes
    val encode_list : t list -> bytes
    val encode_list_aux : t list -> int -> bytes list -> int * bytes list
    val join_list_bytes : Buffer.t -> bytes list -> bytes
    val decode_small_int : bytes -> int
    val decode_small_int_string : string -> int
    val decode_short_string : char -> int -> bytes -> t option
    val decode_long_string : t option
    val decode_short_array : t option
    val decode_long_array : t option
    val decode : bytes -> t option
  end
