(** Send statistics to the stats daemon over UDP

    Synchronous (Sync) and an asynchronous (Lwt) version are provided.
    Module aliasing is occuring here for backwards compatibility.
*)

include Statsd_client_core.C
(** For backwards compatiblity reasons, the config is shared between
    the Sync and Lwt modules. After this backwards compatible
    module is deprecated we will no longer be supporting this shared
    configuration behavior as it is less than ideal. *)
module Sync = Statsd_client_sync
module Lwt = Statsd_client_lwt
[@@deprecated "Use [statsd-client-lwt] opam package for Lwt support"]
