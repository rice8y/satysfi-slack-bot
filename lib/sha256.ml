let rotr x n =
  Int32.logor (Int32.shift_right_logical x n) (Int32.shift_left x (32 - n))

let ( +! ) = Int32.add
let ( ^! ) = Int32.logxor
let ( &! ) = Int32.logand
let ( |! ) = Int32.logor
let lnot32 = Int32.lognot

let ch x y z = (x &! y) ^! (lnot32 x &! z)
let maj x y z = (x &! y) ^! (x &! z) ^! (y &! z)
let big_sigma0 x = rotr x 2 ^! rotr x 13 ^! rotr x 22
let big_sigma1 x = rotr x 6 ^! rotr x 11 ^! rotr x 25
let small_sigma0 x = rotr x 7 ^! rotr x 18 ^! Int32.shift_right_logical x 3
let small_sigma1 x = rotr x 17 ^! rotr x 19 ^! Int32.shift_right_logical x 10

let k =
  [|
    0x428a2f98l; 0x71374491l; 0xb5c0fbcfl; 0xe9b5dba5l; 0x3956c25bl;
    0x59f111f1l; 0x923f82a4l; 0xab1c5ed5l; 0xd807aa98l; 0x12835b01l;
    0x243185bel; 0x550c7dc3l; 0x72be5d74l; 0x80deb1fel; 0x9bdc06a7l;
    0xc19bf174l; 0xe49b69c1l; 0xefbe4786l; 0x0fc19dc6l; 0x240ca1ccl;
    0x2de92c6fl; 0x4a7484aal; 0x5cb0a9dcl; 0x76f988dal; 0x983e5152l;
    0xa831c66dl; 0xb00327c8l; 0xbf597fc7l; 0xc6e00bf3l; 0xd5a79147l;
    0x06ca6351l; 0x14292967l; 0x27b70a85l; 0x2e1b2138l; 0x4d2c6dfcl;
    0x53380d13l; 0x650a7354l; 0x766a0abbl; 0x81c2c92el; 0x92722c85l;
    0xa2bfe8a1l; 0xa81a664bl; 0xc24b8b70l; 0xc76c51a3l; 0xd192e819l;
    0xd6990624l; 0xf40e3585l; 0x106aa070l; 0x19a4c116l; 0x1e376c08l;
    0x2748774cl; 0x34b0bcb5l; 0x391c0cb3l; 0x4ed8aa4al; 0x5b9cca4fl;
    0x682e6ff3l; 0x748f82eel; 0x78a5636fl; 0x84c87814l; 0x8cc70208l;
    0x90befffal; 0xa4506cebl; 0xbef9a3f7l; 0xc67178f2l;
  |]

let int32_of_be_bytes bytes i =
  let b n = Int32.of_int (Char.code (Bytes.get bytes (i + n))) in
  Int32.shift_left (b 0) 24
  |! Int32.shift_left (b 1) 16
  |! Int32.shift_left (b 2) 8
  |! b 3

let set_int32_be bytes i value =
  Bytes.set bytes i (Char.chr (Int32.to_int (Int32.shift_right_logical value 24) land 0xff));
  Bytes.set bytes (i + 1) (Char.chr (Int32.to_int (Int32.shift_right_logical value 16) land 0xff));
  Bytes.set bytes (i + 2) (Char.chr (Int32.to_int (Int32.shift_right_logical value 8) land 0xff));
  Bytes.set bytes (i + 3) (Char.chr (Int32.to_int value land 0xff))

let padded input =
  let len = String.length input in
  let bit_len = Int64.mul (Int64.of_int len) 8L in
  let total_len =
    let with_one_and_len = len + 1 + 8 in
    ((with_one_and_len + 63) / 64) * 64
  in
  let bytes = Bytes.make total_len '\000' in
  Bytes.blit_string input 0 bytes 0 len;
  Bytes.set bytes len '\128';
  for i = 0 to 7 do
    let shift = (7 - i) * 8 in
    let byte =
      Int64.(to_int (logand (shift_right_logical bit_len shift) 0xffL))
    in
    Bytes.set bytes (total_len - 8 + i) (Char.chr byte)
  done;
  bytes

let digest input =
  let bytes = padded input in
  let h =
    [|
      0x6a09e667l; 0xbb67ae85l; 0x3c6ef372l; 0xa54ff53al; 0x510e527fl;
      0x9b05688cl; 0x1f83d9abl; 0x5be0cd19l;
    |]
  in
  let w = Array.make 64 0l in
  for chunk = 0 to (Bytes.length bytes / 64) - 1 do
    let offset = chunk * 64 in
    for i = 0 to 15 do
      w.(i) <- int32_of_be_bytes bytes (offset + (i * 4))
    done;
    for i = 16 to 63 do
      w.(i) <- small_sigma1 w.(i - 2) +! w.(i - 7) +! small_sigma0 w.(i - 15) +! w.(i - 16)
    done;
    let a = ref h.(0)
    and b = ref h.(1)
    and c = ref h.(2)
    and d = ref h.(3)
    and e = ref h.(4)
    and f = ref h.(5)
    and g = ref h.(6)
    and hh = ref h.(7) in
    for i = 0 to 63 do
      let t1 = !hh +! big_sigma1 !e +! ch !e !f !g +! k.(i) +! w.(i) in
      let t2 = big_sigma0 !a +! maj !a !b !c in
      hh := !g;
      g := !f;
      f := !e;
      e := !d +! t1;
      d := !c;
      c := !b;
      b := !a;
      a := t1 +! t2
    done;
    h.(0) <- h.(0) +! !a;
    h.(1) <- h.(1) +! !b;
    h.(2) <- h.(2) +! !c;
    h.(3) <- h.(3) +! !d;
    h.(4) <- h.(4) +! !e;
    h.(5) <- h.(5) +! !f;
    h.(6) <- h.(6) +! !g;
    h.(7) <- h.(7) +! !hh
  done;
  let out = Bytes.make 32 '\000' in
  Array.iteri (fun i word -> set_int32_be out (i * 4) word) h;
  Bytes.unsafe_to_string out

let hex_of_bytes bytes =
  let table = "0123456789abcdef" in
  let out = Bytes.create (String.length bytes * 2) in
  String.iteri
    (fun i ch ->
      let byte = Char.code ch in
      Bytes.set out (i * 2) table.[byte lsr 4];
      Bytes.set out ((i * 2) + 1) table.[byte land 0xf])
    bytes;
  Bytes.unsafe_to_string out

let sha256_hex input = digest input |> hex_of_bytes

let xor_string ch input =
  String.init (String.length input) (fun i ->
      Char.chr (Char.code input.[i] lxor Char.code ch))

let hmac_sha256_hex ~key ~data =
  let block_size = 64 in
  let key = if String.length key > block_size then digest key else key in
  let padded_key = key ^ String.make (block_size - String.length key) '\000' in
  let outer = xor_string '\092' padded_key in
  let inner = xor_string '\054' padded_key in
  digest (outer ^ digest (inner ^ data)) |> hex_of_bytes

let equal_constant_time a b =
  let diff = ref (String.length a lxor String.length b) in
  let max_len = max (String.length a) (String.length b) in
  for i = 0 to max_len - 1 do
    let ca = if i < String.length a then Char.code a.[i] else 0 in
    let cb = if i < String.length b then Char.code b.[i] else 0 in
    diff := !diff lor (ca lxor cb)
  done;
  !diff = 0

