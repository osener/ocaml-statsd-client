include Statsd_client_core.C

include (Statsd_client_core.T with type 'a _r := 'a Async.Deferred.t
                               and type file_descr := Async_unix.Fd.t)

module Dogstatsd : (Dogstatsd.T with type 'a _t := 'a Async.Deferred.t)
