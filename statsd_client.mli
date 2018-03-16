(** Send statistics to the stats daemon over UDP

    Both a synchronous (Sync) and an asynchronous (Lwt) version are provided.
*)
module Sync = Statsd_client_sync.T
module Lwt :
  sig
    val gauge : ?sample_rate:float -> string -> int -> unit Lwt.t
    val timing : ?sample_rate:float -> string -> int -> unit Lwt.t
    val timingf : ?sample_rate:float -> string -> float -> unit Lwt.t
    val update_stats : ?sample_rate:float -> int -> string list -> unit Lwt.t
    val increment : ?sample_rate:float -> string list -> unit Lwt.t
    val decrement : ?sample_rate:float -> string list -> unit Lwt.t
  end

module Async :
  sig
    val gauge : ?sample_rate:float -> string -> int -> unit Async.Deferred.t
    val timing : ?sample_rate:float -> string -> int -> unit Async.Deferred.t
    val timingf : ?sample_rate:float -> string -> float -> unit Async.Deferred.t
    val update_stats : ?sample_rate:float -> int -> string list -> unit Async.Deferred.t
    val increment : ?sample_rate:float -> string list -> unit Async.Deferred.t
    val decrement : ?sample_rate:float -> string list -> unit Async.Deferred.t
  end
