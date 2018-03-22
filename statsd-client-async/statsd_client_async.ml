include Statsd_client_core.Config
module T =  struct
  open Core
  open Async

  let ipaddr () = !Statsd_client_core.Config.ipaddr
  let port () = !Statsd_client_core.Config.port

  type 'a _r = 'a Deferred.t
  let ( >>= ) = ( >>= )
  let return = return
  let catch f error =
    try_with f >>= function
    | Ok d -> return d
    | Error e -> error e
  let list_iter f l = Deferred.List.iter ~f l
  let close fd = Unix.close fd

  type file_descr = Async_unix.Fd.t

  let socket domain typ ip_protocol_num =
    let socket_type =
      match domain, typ, ip_protocol_num with
      | Unix.PF_INET, Unix.SOCK_DGRAM, 17 (* UDP *) -> Socket.Type.udp
      | Unix.PF_INET, Unix.SOCK_STREAM, 6 (* TCP *) -> Socket.Type.tcp
      | _ -> failwith "Protocol not recognized"
    in
    Socket.fd (Socket.create socket_type)

  let inet_addr_exn = function
    | Unix.ADDR_INET (inet_addr, port) ->
      Socket.Address.Inet.create inet_addr ~port
    | Unix.ADDR_UNIX _ -> failwith "Unix domain addresses not supported"

  let sendto fd msg offset msg_length flags socket_address =
    match Udp.sendto () with
    | Error e -> return (-1)
    | Ok send ->
      let buf = msg |> Iobuf.of_string |> Iobuf.read_only in
      let inet_addr = inet_addr_exn socket_address in
      send fd buf inet_addr
      >>| fun () -> msg_length
end

include Statsd_client_core.Make(T)

module Dogstatsd = Dogstatsd.Make(T)
