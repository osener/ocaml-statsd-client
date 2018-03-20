module T : sig
  val gauge : ?sample_rate:float -> string -> int -> unit Lwt.t
  val timing : ?sample_rate:float -> string -> int -> unit Lwt.t
  val timingf : ?sample_rate:float -> string -> float -> unit Lwt.t
  val update_stats : ?sample_rate:float -> int -> string list -> unit Lwt.t
  val increment : ?sample_rate:float -> string list -> unit Lwt.t
  val decrement : ?sample_rate:float -> string list -> unit Lwt.t
end
