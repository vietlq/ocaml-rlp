module Rlp = struct

type t = RlpData of string | RlpList of t list

let encode_non_value = Bytes.make 1 '\x80'

let encode_empty_list = Bytes.make 1 '\xc0'

let encode_bool (b : bool) =
  match b with
  | true -> Bytes.make 1 '\x01'
  | _ -> encode_non_value

let encode_char (c : char) =
  match c with
  | '\x00'..'\x7f' -> begin let b = Bytes.create 1 in Bytes.set b 0 c; b end
  | c -> begin let b = Bytes.create 2 in Bytes.set b 0 '\x81'; Bytes.set b 1 c; b end

let big_endian_bytes_of_uint num =
  let rec aux total_bytes list_bytes n =
    if n < 0 then failwith "Must be non-negative!"
    else if n = 0 then (total_bytes, list_bytes)
    else begin
      aux (total_bytes + 1) (char_of_int (n mod 256) :: list_bytes) (n / 256)
    end
  in
  let total_bytes, list_bytes = aux 0 [] num in
  let bytes = Bytes.create total_bytes in
  List.iteri (fun idx v -> Bytes.set bytes idx v) list_bytes;
  bytes

(* https://ethereum.github.io/yellowpaper/paper.pdf *)
(* https://github.com/ethereum/wiki/wiki/RLP *)
(* https://medium.com/coinmonks/data-structure-in-ethereum-episode-1-recursive-length-prefix-rlp-encoding-decoding-d1016832f919 *)
(* https://ethereum.stackexchange.com/questions/30518/does-rlp-specify-integer-encoding *)
let encode_int num =
  let aux_bytes = big_endian_bytes_of_uint num in
  match Bytes.length aux_bytes with
  | 0 -> encode_non_value
  | 1 -> if num = 0 then encode_non_value
    else if num < 128 then aux_bytes
    else Bytes.cat (Bytes.make 1 '\x81') aux_bytes
  | n -> Bytes.cat (Bytes.make 1 @@ char_of_int (0x80 + n)) aux_bytes

let encode_bytes inbytes =
  match Bytes.length inbytes with
  | 0 -> encode_non_value
  | 1 -> encode_char @@ Bytes.get inbytes 0
  | len when len < 56 ->
    begin
      let bytes = Bytes.create 1 in
      Bytes.set bytes 0 @@ char_of_int (0x80 + len);
      Bytes.cat bytes inbytes
    end
  | len ->
    begin
      let bytes_of_int = big_endian_bytes_of_uint len in
      let len_of_int = Bytes.length bytes_of_int in
      let bytes = Bytes.create 1 in
      Bytes.set bytes 0 @@ char_of_int (0xb7 + len_of_int);
      let bytes = Bytes.cat bytes bytes_of_int in
      Bytes.cat bytes inbytes
    end

let encode_string instring =
  encode_bytes @@ Bytes.of_string instring

let rec encode inobj =
  match inobj with
  | RlpData data -> encode_string data
  | RlpList alist -> encode_list alist
and encode_list alist =
  let total_len, acc = encode_list_aux alist 0 [] in
  match total_len with
  | n when n < 0 -> failwith "Length must be non-negative!"
  | 0 -> encode_empty_list
  | total_len ->
    let bytes = join_list_bytes (Buffer.create total_len) (List.rev acc) in
    assert(Bytes.length bytes == 1 + total_len);
    bytes
and encode_list_aux alist total_len acc =
  match alist with
  | [] -> total_len, acc
  | [x] ->
    let encoded_item = encode x in
    (total_len + (Bytes.length encoded_item)), encoded_item :: acc
  | h :: tl ->
    let encoded_item = encode h in
    encode_list_aux
      tl
      (total_len + (Bytes.length encoded_item))
      (encoded_item :: acc)
and join_list_bytes buffer list_bytes =
  let () = List.iter (fun x -> Buffer.add_bytes buffer x) list_bytes in
  let total_len = Buffer.length buffer in
  match total_len with
  | n when n < 56 ->
    let prefix = Bytes.make 1 (char_of_int (0xc0 + n)) in
    Bytes.cat prefix @@ Buffer.to_bytes buffer
  | n ->
    let payload_len_bytes = big_endian_bytes_of_uint n in
    let payload_len_bytes_len = Bytes.length payload_len_bytes in
    let prefix = Bytes.make 1 (char_of_int (0xf7 + payload_len_bytes_len)) in
    Bytes.cat (Bytes.cat prefix payload_len_bytes) @@ Buffer.to_bytes buffer
end
