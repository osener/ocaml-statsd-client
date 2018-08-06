module type T = sig
  include Dogstatsd_types.Types
end

module Make : functor (IO: Statsd_client_core.IO)
  -> (T with type 'a _t := 'a IO._r)
