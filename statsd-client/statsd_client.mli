(** Send statistics to the stats daemon over UDP

    Synchronous (Sync) and an asynchronous (Lwt) version are provided.
    Module aliasing is occuring here for backwards compatibility.
*)
module Sync = Statsd_client_sync.T
module Lwt = Statsd_client_lwt.T
[@@deprecated "Use [statsd-client-lwt] opam package for Lwt support"]
