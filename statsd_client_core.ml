let ipaddr = ref None
let port = ref None

let log_debug = ref (fun s -> ())
let log_error = ref (fun s -> Printf.eprintf "[error] %s\n%!" s)

module type IO = sig
  val ipaddr         : unit -> string option
    (** Get the host ip address.  Allows for dynamic setting *)
  val port           : unit -> int option
    (** Get the port.  Allows for dynamic setting *)

  (** Define these for Lwt or non-Lwt use *)
  type 'a _r
  val (>>=)          : 'a _r -> ('a -> 'b _r) -> 'b _r
  val catch          : (unit -> 'a _r) -> (exn -> 'a _r) -> 'a _r
  val return         : 'a -> 'a _r
  val list_iter      : ('a -> unit _r) -> 'a list -> unit _r

  type file_descr
  val socket : Unix.socket_domain -> Unix.socket_type -> int -> file_descr
  val sendto :
    file_descr ->
    string -> int -> int ->
    Unix.msg_flag list -> Unix.sockaddr -> int _r
  val close : file_descr -> unit _r
end

module type T = sig
  type 'a _r
  type file_descr
  val ( >>= ) : 'a _r -> ('a -> 'b _r) -> 'b _r
  val socket_ref : file_descr option ref
  val send_with_ipaddr_and_port :
    string -> int -> float -> (string * string) list -> unit _r
  val send : ?sample_rate:float -> (string * string) list -> unit _r
  val gauge : ?sample_rate:float -> string -> int -> unit _r
  val timing : ?sample_rate:float -> string -> int -> unit _r
  val timingf : ?sample_rate:float -> string -> float -> unit _r
  val update_stats : ?sample_rate:float -> int -> string list -> unit _r
  val increment : ?sample_rate:float -> string list -> unit _r
  val decrement : ?sample_rate:float -> string list -> unit _r
end

module Make (U : IO) :
  (T with type 'a _r := 'a U._r and type file_descr := U.file_descr) = struct

  let (>>=) = U.(>>=)

  (** The socket reference used to send udp *)
  let socket_ref = ref None

  (** Send the the stats over UDP *)
  let send_with_ipaddr_and_port ipaddr port sample_rate data =
    if sample_rate >= 1.0 || sample_rate >= Random.float 1.0 then
      U.catch (fun () ->
        (* getprotobyname call hangs on Mac OS X 10.8.3 -->> Removed  *)
        (* U.getprotobyname "udp" >>= fun protocol_entry ->
        let proto = protocol_entry.Unix.p_proto in
        *)
        let proto = 17 (* UDP *) in
        (* Get or make the socket *)
        let socket =
          match !socket_ref with
            | Some s -> s
            | None   ->
              !log_debug "Creating statsd socket";
              let s = U.socket Unix.PF_INET Unix.SOCK_DGRAM proto in
              socket_ref := Some s;
              s
        in
        let portaddr = Unix.ADDR_INET (Unix.inet_addr_of_string ipaddr, port) in
        (* Map sample_rate to the string statsd expects *)
        let sample_rate =
          if sample_rate >= 1.0 then
            ""  (* not sampling *)
          else
            Printf.sprintf "|@%f" sample_rate
        in
        U.list_iter
          (fun (stat, value) ->
            let msg = Printf.sprintf "%s:%s%s" stat value sample_rate in
            U.sendto socket msg 0 (String.length msg) [] portaddr
            >>= (fun retval ->
              if retval < 0 then
                begin
                  (* Ran into an error, clear the reference
                     and close the socket *)
                  socket_ref := None;
                  U.catch
                    (fun () ->
                      !log_debug
                        (Printf.sprintf "Closing statsd socket: %d" retval);
                      U.close socket >>= U.return)
                    (fun _e -> U.return ());
                end
              else U.return ()
            )
          )
          data
      ) (fun _e -> U.return ()
      )
    else U.return ()

  let send ?(sample_rate = 1.0) data =
    match U.ipaddr (), U.port () with
      | None, _ | _, None ->
        !log_error
          "Statsd_client.send: \
           uninitialized Statsd_client.host or Statsd_client.port";
        U.return ()
      | Some ipaddr, Some port ->
        send_with_ipaddr_and_port ipaddr port sample_rate data

  let gauge ?sample_rate stat v =
    send ?sample_rate [stat, Printf.sprintf "%d|g" v]

  (** Log timing info. time is an int of milliseconds. *)
  let timing ?sample_rate stat time =
    send ?sample_rate [stat, Printf.sprintf "%d|ms" time]

  (** Log timing info. time is a float of seconds which will
      be converted to milliseconds. *)
  let timingf ?sample_rate stat time =
    timing ?sample_rate stat (int_of_float (time *. 1000.))

  (** Update a list of counter stats by some delta *)
  let update_stats ?sample_rate delta stats =
    let delta = Printf.sprintf "%d|c" delta in
    send ?sample_rate (List.map (fun stat -> stat, delta) stats)

  (** Increment a list of counter stats by one *)
  let increment ?sample_rate stats =
    update_stats ?sample_rate 1 stats

  (** Decrement a list of counter stats by one *)
  let decrement ?sample_rate stats =
    update_stats ?sample_rate (-1) stats
end
