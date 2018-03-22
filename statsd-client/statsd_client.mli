(** Send statistics to the stats daemon over UDP

    Synchronous (Sync) and an asynchronous (Lwt) version are provided.
    Module aliasing is occuring here for backwards compatibility.
*)

include Statsd_client_core.C
module Sync = Statsd_client_sync
module Lwt = Statsd_client_lwt
[@@deprecated "Use [statsd-client-lwt] opam package for Lwt support"]
