module type C = sig
  val ipaddr : string option ref
  val port : int option ref

  val log_debug : (string -> unit) ref
  (** Default: do nothing. *)

  val log_error : (string -> unit) ref
  (** Default: log to stderr. *)
end

module Config : C = struct
  let ipaddr = ref None
  let port = ref None

  let log_debug = ref (fun s -> ())
  let log_error = ref (fun s -> Printf.eprintf "[error] %s\n%!" s)
end

let fmt_guage = Printf.sprintf "%d|g"
let fmt_time = Printf.sprintf "%d|ms"
let fmt_counting = Printf.sprintf "%d|c"
let sec_to_ms time = int_of_float (time *. 1000.)
let fmt_sample_rate sample_rate =
  if sample_rate >= 1.0 then
    ""  (* not sampling *)
  else
    Printf.sprintf "|@%f" sample_rate

let fmt_statsd_payload ~sample_rate (metric_name, value_type) =
  Printf.sprintf "%s:%s%s" metric_name value_type (fmt_sample_rate sample_rate)

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
  val send_with_ipaddr_and_port : string -> int -> string list -> unit _r
  val send : data:string list -> unit _r
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
  let send_with_ipaddr_and_port ipaddr port data =
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
            !Config.log_debug "Creating statsd socket";
            let s = U.socket Unix.PF_INET Unix.SOCK_DGRAM proto in
            socket_ref := Some s;
            s
        in
        let portaddr = Unix.ADDR_INET (Unix.inet_addr_of_string ipaddr, port) in
        U.list_iter
          (fun msg ->
             U.sendto socket msg 0 (String.length msg) [] portaddr
             >>= (fun retval ->
                 if retval < 0 then
                   begin
                     (* Ran into an error, clear the reference
                        and close the socket *)
                     socket_ref := None;
                     U.catch
                       (fun () ->
                          !Config.log_debug
                            (Printf.sprintf "Closing statsd socket: %d" retval);
                          U.close socket >>= U.return)
                       (fun _e -> U.return ());
                   end
                 else U.return ()
               )
          )
          data
      ) (fun _e -> U.return ())

  let send ~data =
    match U.ipaddr (), U.port () with
    | None, _ | _, None ->
      !Config.log_error
        "Statsd_client.send: \
         uninitialized Statsd_client.host or Statsd_client.port";
      U.return ()
    | Some ipaddr, Some port ->
      send_with_ipaddr_and_port ipaddr port data

  let fmt_data ?(sample_rate = 1.0) data =
    if sample_rate >= 1.0 || sample_rate >= Random.float 1.0
    then List.map (fmt_statsd_payload ~sample_rate) data
    else []

  let gauge ?sample_rate stat v =
    send (fmt_data ?sample_rate [stat, fmt_guage v])

  (** Log timing info. time is an int of milliseconds. *)
  let timing ?sample_rate stat time =
    send (fmt_data ?sample_rate [stat, fmt_time time])

  (** Log timing info. time is a float of seconds which will
      be converted to milliseconds. *)
  let timingf ?sample_rate stat time =
    timing ?sample_rate stat (sec_to_ms time)

  (** Update a list of counter stats by some delta *)
  let update_stats ?sample_rate delta stats =
    let delta = fmt_counting delta in
    send (fmt_data ?sample_rate (List.map (fun stat -> stat, delta) stats))

  (** Increment a list of counter stats by one *)
  let increment ?sample_rate stats =
    update_stats ?sample_rate 1 stats

  (** Decrement a list of counter stats by one *)
  let decrement ?sample_rate stats =
    update_stats ?sample_rate (-1) stats
end
