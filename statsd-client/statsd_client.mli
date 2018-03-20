(** Send statistics to the stats daemon over UDP

    Synchronous (Sync) and two asynchronous (Lwt & Async) versions are provided.
    Module aliasing is occuring here for backwards compatibility.
*)
module Sync = Statsd_client_sync.T
module Lwt = Statsd_client_lwt.T
module Async = Statsd_client_async.T
