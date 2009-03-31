
open Sesame

type 'a transcript = 
{ 
  exons : (int * int) list; 
  lo : int; 
  hi : int;
  chr : string;
  info : 'a
}

type 'a t = 'a transcript list

module II = struct
  type t = int * int
  let compare = Pervasives.compare
end

module SIIMap = MMap.Make(String)(II)
module SSMap = MMap.Make(String)(String)

let of_composite_channel 
    ?(chr_map=identity) 
    ?(increment_lo_hi=(0,0)) 
    ic = 
  let f acc l = 
    let lst = String.nsplit l "\t" in
    let inclo,inchi = increment_lo_hi in
    let (nm,chr,st,fn) = 
      List.nth lst 0,
      chr_map (List.nth lst 1),
      int_of_string (List.nth lst 2) + inclo,
      int_of_string (List.nth lst 3) + inchi
    in
    let g (nm,chr,st,fn) prev = match prev with 
      | None -> 
          {
            exons = [st,fn]; 
            lo = st;
            hi= fn;
            chr = chr;
            info = nm;
          }
      | Some trx -> 
          {
            exons = (st,fn)::(trx.exons);
            lo = if st < trx.lo then st else trx.lo;
            hi = if fn > trx.hi then fn else trx.hi;
            chr = chr;
            info = nm;
          }
    in
    SSMap.add_with nm chr (g (nm,chr,st,fn)) acc
  in
  let ans = Lines.fold_channel f SSMap.empty ic in
  let folder k1 k2 acc elem = elem::acc in
  List.rev (SSMap.fold folder [] ans)

let of_composite_file 
    ?(chr_map=identity)
    ?(increment_lo_hi=(0,0))
    file = 
  try_finally (of_composite_channel ~chr_map ~increment_lo_hi) close_in 
    (open_in file)

let of_bed_channel ?(chr_map=identity) ?(increment_lo_hi=(1,0)) ic = 
  let bed = Bed.to_list (Bed.of_channel ~chr_map ~increment_lo_hi ic) in
  let f acc (chr,s,f) = 
    {
      exons = [s,f];
      lo = s;
      hi = f;
      chr = chr_map chr;
      info = "";
    }::acc 
  in
  List.rev (List.fold_left f [] bed)

let of_bed_file ?(chr_map=identity) ?(increment_lo_hi=(1,0)) file = 
  try_finally (of_bed_channel ~chr_map ~increment_lo_hi) close_in (open_in file)

let all_probes_in 
    (trx_lst:'a t) 
    (prbs: (string * int * int * float) list) 
    : ('a * float array) t =
  let insert x prev = match prev with None -> [x] | Some l -> x::l in
  let siimap_of_exons = 
    let f acc trx = SIIMap.add trx.chr (trx.lo,trx.hi) (trx.exons,trx.info) acc in
    List.fold_left f SIIMap.empty trx_lst
  in 
  let stringmap_of_intervaltrees = 
    let f acc trx = StringMap.add_with trx.chr (insert (trx.lo,trx.hi)) acc in
    let ans = List.fold_left f StringMap.empty trx_lst in
    StringMap.map IntervalTree.create ans
  in
  let f acc (chr,s,f,v) = 
    let itree = StringMap.find chr stringmap_of_intervaltrees in
    let trxs = IntervalTree.within itree (s,f) in
    let g accu trx = 
      let (exons,info) = SIIMap.find chr trx siimap_of_exons in
      let g_insert (info,u) prev = 
        match prev with 
          | None -> info,[v] 
          | Some (i,lst) -> (assert (i = info); i,(v::lst))
      in
      match IntervalTree.within (IntervalTree.create exons) (s,f) with
        | [] -> accu
        | a::b -> SIIMap.add_with chr trx (g_insert (info,v)) accu
    in
    List.fold_left g acc trxs
  in
  let ans = List.fold_left f SIIMap.empty prbs in
  let ans = SIIMap.map (fun (info,lst) -> (info,Array.of_list lst)) ans in
  let f acc trx = 
    try 
      {
        exons = trx.exons;
        lo = trx.lo;
        hi = trx.hi;
        chr = trx.chr;
        info = SIIMap.find trx.chr (trx.lo,trx.hi) ans
      }::acc
    with Not_found -> acc
  in
  List.rev (List.fold_left f [] trx_lst)

let all_points_in 
    (trx_lst:'a t) 
    (points: (string * int * float) list)
    : ('a * float array) t =
  let insert x prev = match prev with None -> [x] | Some l -> x::l in
  let siimap_of_exons = 
    let f acc trx = SIIMap.add trx.chr (trx.lo,trx.hi) (trx.exons,trx.info) acc in
    List.fold_left f SIIMap.empty trx_lst
  in 
  let stringmap_of_intervaltrees = 
    let f acc trx = StringMap.add_with trx.chr (insert (trx.lo,trx.hi)) acc in
    let ans = List.fold_left f StringMap.empty trx_lst in
    StringMap.map IntervalTree.create ans
  in
  let f acc (chr,bp,v) = 
    let itree = StringMap.find chr stringmap_of_intervaltrees in
    let trxs = IntervalTree.within_pt itree bp in
    let g accu trx = 
      let (exons,info) = SIIMap.find chr trx siimap_of_exons in
      let g_insert (info,u) prev = 
        match prev with 
          | None -> info,[v] 
          | Some (i,lst) -> (assert (i = info); i,(v::lst))
      in
      match IntervalTree.within_pt (IntervalTree.create exons) bp with
        | [] -> accu
        | a::b -> SIIMap.add_with chr trx (g_insert (info,v)) accu
    in
    List.fold_left g acc trxs
  in
  let ans = List.fold_left f SIIMap.empty points in
  let ans = SIIMap.map (fun (info,lst) -> (info,Array.of_list lst)) ans in
  let f acc trx = 
    try
      {
        exons = trx.exons;
        lo = trx.lo;
        hi = trx.hi;
        chr = trx.chr;
        info = SIIMap.find trx.chr (trx.lo,trx.hi) ans
      }::acc
    with Not_found -> acc
  in
  List.rev (List.fold_left f [] trx_lst)