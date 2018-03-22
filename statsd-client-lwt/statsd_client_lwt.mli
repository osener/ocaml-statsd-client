include Statsd_client_core.C

include (Statsd_client_core.T with type 'a _r := 'a Lwt.t
                               and type file_descr := Lwt_unix.file_descr)

module Dogstatsd : (Dogstatsd.T with type 'a _t := 'a Lwt.t)
