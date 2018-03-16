module T : sig
  val gauge : ?sample_rate:float -> string -> int -> unit Async.Deferred.t
  val timing : ?sample_rate:float -> string -> int -> unit Async.Deferred.t
  val timingf : ?sample_rate:float -> string -> float -> unit Async.Deferred.t
  val update_stats : ?sample_rate:float -> int -> string list -> unit Async.Deferred.t
  val increment : ?sample_rate:float -> string list -> unit Async.Deferred.t
  val decrement : ?sample_rate:float -> string list -> unit Async.Deferred.t
end
