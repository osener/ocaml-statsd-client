module T = Statsd_client_core.Make (
  struct
    let ipaddr () = !Statsd_client_core.ipaddr
    let port () = !Statsd_client_core.port

    type 'a _r = 'a Lwt.t
    let ( >>= ) = Lwt.bind
    let catch = Lwt.catch
    let return = Lwt.return
    let list_iter = Lwt_list.iter_p

    include Lwt_unix
    let socket dom typ proto = Lwt_unix.socket dom typ proto
  end
)
