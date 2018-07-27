include Statsd_client_core.Config
include Statsd_client_core.Make (
  struct
    let ipaddr () = !Statsd_client_core.Config.ipaddr
    let port () = !Statsd_client_core.Config.port

    type 'a _r = 'a
    let ( >>= ) t f = f t
    let catch f error = try f () with e -> error e
    let return x = x
    let list_iter f lst = List.iter f lst

    include Unix
    let sendto = sendto_substring
    let socket dom typ proto = Unix.socket dom typ proto
  end
)
