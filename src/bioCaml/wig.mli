(** WIG data.
    
    Internal representation of coordinates always assumes the first position on a chromosome is numbered 1. Also, integer ranges are always closed; the range [\[1, 10\]] is the set of integers from 1 to 10 inclusive of 0 and 10. WIG data can be in three formats---bed, variable-step, or fixed-step---and unfortunately each has different conventions as follows:
    - Bed format requires half-open intervals [\[low, high\)]. Thus 1 is subtracted from the high value when parsing. The line ["chrI 0 10 3.14"] is parsed to [("chrI", 0, 9, 3.14)].
    - Variable-step format numbers the first position 1. Thus 1 is subtracted from the low value when parsing. The line ["1 3.14"] is parsed to [(0, 3.14)].
    - Fixed-step format numbers the first position 1. Thus 1 is subtracted from the start coordinate given in header lines. The header line ["fixedStep chrom=chrI start=1 step=100 span=30"] is parsed to [("chrI", 0, 100, 30)].
    
    The inverse is done for printing routines. You are freed from these details if you always use this module to parse and print.
    
    All parsers allow columns (fields) on a line to be separated by any combination of space, tab, or carriage return characters. Printers always separate columns with a single tab. Tag-value pairs must be in the form "tag=value" with no space around the '='.
*)

type pt = string * int * int * float
    (** A data point is a 4-tuple [(chr,lo,hi,x)], where [x] is the value assigned to interval [\[lo, hi\]], inclusive of end-points, on chromosome [chr]. *)
    
type t
    (** Type of WIG data. Can be thought of as a collection of [pt]'s. Coordinates of data points are not allowed to overlap, for each chromosome. *)
    
type format = Bed | VariableStep | FixedStep
    (** The three formats in which WIG data can be specified. *)

exception Bad of string
  
val of_list : ?sort:bool -> pt list -> t
  (** Construct WIG data from given [pt]'s. Default value of [sort] is true, which means input will be sorted before constructing [t]. For efficiency, if input already sorted, by chromosome and then by coordinates, you should set [sort] to false to avoid unnecessarily sorting. Raise [Bad] if valid data set cannot be constructed, or if [sort = false] but data not already sorted. *)

val to_list : t -> pt list
  (** Extract data as a flat list. Guaranteed to be in order by chromosome name, and then coordinate. *)
  
val iter : (pt -> unit) -> t -> unit
val fold : ('a -> pt -> 'a) -> 'a -> t -> 'a

val to_file : ?fmt:format -> t -> string -> unit
  (** [to_file ~fmt t file] prints [t] to [file]. Printing is in most efficient format possible by default. Optional [fmt] argument forces printing in a specific format. Requesting [VariableStep] or [FixedStep] may raise [Failure] if given data cannot be represented in those formats. *)

val to_channel : ?fmt:format -> t -> out_channel -> unit
  (** Like [to_file] but print to channel. *)

val compact : t -> t
  (** Compact data to most memory efficient format possible. *)

val to_format : format -> t -> t option
  (** [to_format fmt t] converts internal representation of [t] to format [fmt] if possible, or returns None otherwise. *)

val of_bed_channel : ?chr_map:(string -> string) -> ?header:bool 
  -> ?increment_lo_hi:(int * int) -> in_channel -> t
val of_bed_file : ?chr_map:(string -> string) -> ?header:bool 
  -> ?increment_lo_hi:(int * int) -> string -> t


(*  
val of_channel : in_channel -> t
val of_file : string -> t
*)

(**/**)

module B : sig
  type datum
      (** chromosome, lo, hi, value. *)
      
  type s
      (** Representation of bed formatted WIG data that supports efficient appending. *)
      
  val empty : s
    (** The empty data set. *)
    
  val singleton : datum -> s
    (** Return the data set with the single given data point. *)
    
  val append_datum : s -> datum -> s
    (** [append_datum s x] appends [x] to the end of [s]. Raise [Bad] if [x] cannot be added or is ill-formed. *)
    
  val complete : s -> t
    
  val datum_of_string : string -> datum
    (** Raise [Bad] if any errors. E.g. ["chrI 0 10 3.14"] is parsed to [("chrI", 0, 9, 3.14)]. *)
    
  val datum_to_string : datum -> string
    (** E.g. [("chrI", 0, 9, 3.14)] is converted to ["chrI\t0\t10\t3.14"]. *)
end

module V : sig
  type header
      (** Header line provides a chromosome name and span. *)

  type datum
      (** Each data line provides a low coordinate and data value. *)

  type section
      (** Data for one chromosome. *)
      
  type s
      (** Type of WIG data in variable-step format. *)
      
  val get_span : s -> int option
    (** Return the span, or None if [s] is empty. *)
    
  val header_of_string : string -> header
    (** Raise [Bad] if any errors. *)
    
  val datum_of_string : string -> datum
    (** Raise [Bad] if any errors. E.g. ["1 3.14"] is parsed to [(0, 3.14)]. *)
    
  val empty : s
    (** Empty data set. *)

  val empty_section : header -> section
    
  val append_datum : section -> datum -> section
    (** Append given data point. Raise [Bad] if this cannot be done. *)

  val append_section : s -> section -> s
    (** Append section. Raise [Bad] if this cannot be done. *)

  val complete : s -> t
    (** Raise [Bad] if it does not make sense to be done adding data. This is possible for example if a new header was just set but no datum were appened. *)
    
  val header_to_string : header -> string

  val datum_to_string : datum -> string
    (** E.g. [(0, 3.14)] is converted to ["1\t3.14"]. *)

end

module F : sig
  type header
      (** Header line provides a chromosome name, start coordinate, step size, and span. *)
      
  type datum
      (** Each data line provides just a float data value. *)
      
  type section
      (** Data for one chromosome. *)
      
  type s
      (** Type of WIG data in fixed-step format. *)

  val get_span : s -> int option
      
  val header_of_string : string -> header
    (** Raise [Bad] if any errors. E.g. ["fixedStep chrom=chrI start=1 step=100 span=30"] is parsed to [("chrI", 0, 100, 30)]. *)
    
  val datum_of_string : string -> datum
    (** Raise [Bad] if any errors. *)
    
  val empty : s
    (** Empty data set. *)

  val empty_section : header -> section
        
  val append_datum : section -> datum -> section
    (** Append given data point. Raise [Bad] if this cannot be done. *)
    
  val append_section : s -> section -> s
    (** Append section. Raise [Bad] if this cannot be done. *)

  val complete : s -> t
    (** Raise [Bad] if it does not make sense to be done adding data. This is possible for example if a new header was just set but no datum were appened. *)
    
  val header_to_string : header -> string
    (** E.g. [("chrI", 0, 100, 30)] is converted to ["fixedStep\tchrom=chrI\tstart=1\tstep=100\tspan=30"]. *)
    
  val datum_to_string : datum -> string
end
