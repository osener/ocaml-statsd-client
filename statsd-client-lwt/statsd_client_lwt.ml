include Statsd_client_core.Config
module T = struct
  let ipaddr () = !Statsd_client_core.Config.ipaddr
  let port () = !Statsd_client_core.Config.port

  type 'a _r = 'a Lwt.t
  let ( >>= ) = Lwt.bind
  let catch = Lwt.catch
  let return = Lwt.return
  let list_iter = Lwt_list.iter_p

  include Lwt_unix
  let socket dom typ proto = Lwt_unix.socket dom typ proto
end

include Statsd_client_core.Make(T)
module Dogstatsd = Dogstatsd.Make(T)
