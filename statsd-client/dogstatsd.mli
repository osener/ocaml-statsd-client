module type T = Dogstatsd_intf.S

module Make : functor (IO: Statsd_client_core.IO)
  -> (T with type 'a _t := 'a IO._r)
