open Core_kernel

type t = Bytes.t
exception Bad of string
let raise_bad msg = raise (Bad msg)

module Set = Char.Set

let codes =
  let codes =
    ['A';'C';'G';'T';'U';'R';'Y';'K';'M';'S';'W';'B';'D';'H';'V';'N';'X'] in
  List.fold_left ~f:(fun s e -> Set.add s e) ~init:Set.empty codes

let is_nucleic_acid c = Set.mem codes (Char.uppercase c)

let too_long =
  ("sequence length exceeds " ^ (string_of_int Caml.Sys.max_string_length))
let bad_acid c = "invalid sequence element " ^ String.of_char c

let of_buffer b =
  let n = Buffer.length b in
  if n > Caml.Sys.max_string_length then raise_bad too_long
  else
    let ans = Bytes.create n in
    for i = 0 to n-1 do
      let c = Char.uppercase (Buffer.nth b i) in
      if is_nucleic_acid c then Bytes.set ans i c
      else raise_bad (bad_acid (Buffer.nth b i))
    done;
    ans

let of_string b =
  let n = String.length b in
  let ans = Bytes.create n in
  for i = 0 to n-1 do
    let c = Char.uppercase b.[i] in
    if is_nucleic_acid c then Bytes.set ans i c
    else raise_bad (bad_acid b.[i])
  done;
  ans

let of_buffer_unsafe b = Buffer.contents b |> String.uppercase |> Bytes.From_string.subo
let of_string_unsafe s = String.copy s |> String.uppercase |> Bytes.From_string.subo

let to_string t = Bytes.To_string.subo t
let nth t i = Bytes.get t (i-1)
let length = Bytes.length
(* FIXME: conform Core "t must come first" and have the same names *)
let fold_lefti f init s =
  let r = ref init in
  for i = 0 to Bytes.length s do
    r := f !r i (Bytes.get s i)
  done;
  !r

let fold_left f init s =
  let r = ref init in
  for i = 0 to Bytes.length s do
    r := f !r (Bytes.get s i)
  done;
  !r

(* FIXME: should have the same semantics as Bytes.slice (otherwise
   this is error prone). *)
let slice first last t =
  if first < 1 || last > Bytes.length t || first > last then
    failwith "requesting invalid slice from sequence"
  else
    String.slice (Bytes.To_string.subo t) (first - 1) last
    |> Bytes.From_string.subo
