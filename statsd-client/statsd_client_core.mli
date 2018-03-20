val ipaddr : string option ref
val port : int option ref

val log_debug : (string -> unit) ref
  (** Default: do nothing. *)

val log_error : (string -> unit) ref
  (** Default: log to stderr. *)

module type IO = sig
  val ipaddr         : unit -> string option
    (** Get the host ip address.  Allows for dynamic setting *)
  val port           : unit -> int option
    (** Get the port.  Allows for dynamic setting *)

  (** Define these for Lwt or non-Lwt use *)
  type 'a _r
  val (>>=)          : 'a _r -> ('a -> 'b _r) -> 'b _r
  val catch          : (unit -> 'a _r) -> (exn -> 'a _r) -> 'a _r
  val return         : 'a -> 'a _r
  val list_iter      : ('a -> unit _r) -> 'a list -> unit _r

  type file_descr
  val socket : Unix.socket_domain -> Unix.socket_type -> int -> file_descr
  val sendto :
    file_descr ->
    string -> int -> int ->
    Unix.msg_flag list -> Unix.sockaddr -> int _r
  val close : file_descr -> unit _r
end

module type T = sig
  type 'a _r
  type file_descr
  val ( >>= ) : 'a _r -> ('a -> 'b _r) -> 'b _r
  val socket_ref : file_descr option ref
  val send_with_ipaddr_and_port :
    string -> int -> float -> (string * string) list -> unit _r
  val send : ?sample_rate:float -> (string * string) list -> unit _r
  val gauge : ?sample_rate:float -> string -> int -> unit _r
  val timing : ?sample_rate:float -> string -> int -> unit _r
  (** Log timing info. time is an int of milliseconds. *)
  val timingf : ?sample_rate:float -> string -> float -> unit _r
  (** Log timing info. time is a float of seconds which will
      be converted to milliseconds. *)
  val update_stats : ?sample_rate:float -> int -> string list -> unit _r
  (** Update a list of counter stats by some delta *)
  val increment : ?sample_rate:float -> string list -> unit _r
  (** Increment a list of counter stats by one *)
  val decrement : ?sample_rate:float -> string list -> unit _r
  (** Decrement a list of counter stats by one *)
end

module Make : functor (U : IO)
  -> (T with type 'a _r := 'a U._r and type file_descr := U.file_descr)
