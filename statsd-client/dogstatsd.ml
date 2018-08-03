open Sexplib
open Sexplib.Std

let value_or_empty_str f = function
  | None -> ""
  | Some v -> f v

module Tag = struct
  type t = string * string [@@deriving sexp] (** (tag-name, value) *)

  let datagram_fmt l =
    let format_tag (name, value) = Printf.sprintf "%s:%s" name value in
    Printf.sprintf "|#%s" (String.concat "," (List.map format_tag l))
end

(** https://docs.datadoghq.com/developers/dogstatsd/#metrics *)
module Metric = struct

  module Value_type = struct

    module Counter = struct
      type t =
        [ `Increment (** Increments by 1 *)
        | `Decrement (** Decrements by 1 *)
        | `Value of int ] 
        [@@deriving sexp] 
        (** Decrements or Increments by the given value *)

      let datagram_fmt = function
        | `Increment -> "1"
        | `Decrement -> "-1"
        | `Value v -> Printf.sprintf "%d" v
    end

    type t =
      [ `Counter of Counter.t
      | `Gauge of float
      | `Timer of float
      | `Histogram of int
      | `Set of int ] 
      [@@deriving sexp]

    let datagram_fmt = function
      | `Counter c -> Printf.sprintf "%s|c" (Counter.datagram_fmt c)
      | `Gauge g -> Printf.sprintf "%f|g" g
      | `Timer t -> Printf.sprintf "%f|ms" t
      | `Histogram h -> Printf.sprintf "%d|h" h
      | `Set s -> Printf.sprintf "%d|s" s
  end

  type t =
    { metric_name : string
    ; metric : Value_type.t
    ; sample_rate : float option
    (** Sample rates only work with `Counter, `Histogram and `Timer typ
        metrics *)
    ; tags : Tag.t list }
    [@@deriving sexp]

  let sample_rate_datagram_fmt = function
    | None -> ""
    | Some s -> Printf.sprintf "|@%f" s

  (** For datagram format see:
      https://docs.datadoghq.com/developers/dogstatsd/#metrics-1 *)
  let datagram_fmt t =
    Printf.sprintf "%s:%s%s%s"
      t.metric_name
      (Value_type.datagram_fmt t.metric)
      (sample_rate_datagram_fmt t.sample_rate)
      (Tag.datagram_fmt t.tags)
end

(** https://docs.datadoghq.com/developers/dogstatsd/#service-checks *)
module ServiceCheck = struct
  module Status = struct
    type t =
      [ `Ok
      | `Warning
      | `Critical
      | `Unknown ] 
      [@@deriving sexp]

    let datagram_fmt = function
      | `Ok -> "0"
      | `Warning -> "1"
      | `Critical -> "2"
      | `Unknown -> "3"
  end
  type t =
    { name : string
    ; status : Status.t
    ; timestamp : int option
    ; hostname : string option
    ; tags : Tag.t list
    ; message: string option } 
    [@@deriving sexp]

  (** For datagram format see:
      https://docs.datadoghq.com/developers/dogstatsd/#service-checks-1 *)
  let datagram_fmt t =
    Printf.sprintf "_sc|%s|%s%s%s%s%s"
      t.name
      (Status.datagram_fmt t.status)
      (value_or_empty_str (Printf.sprintf "|d:%d") t.timestamp)
      (value_or_empty_str (Printf.sprintf "|h:%s") t.hostname)
      (Tag.datagram_fmt t.tags)
      (value_or_empty_str (Printf.sprintf "|m:%s") t.message)
end

(** https://docs.datadoghq.com/developers/dogstatsd/#events *)
module Event = struct

  module Priority = struct
    type t = [ `Normal | `Low ] [@@deriving sexp]

    let datagram_fmt t =
      let to_string = function
        | `Normal -> "normal"
        | `Low -> "low"
      in
      Printf.sprintf "|p:%s" (to_string t)

  end

  module Alert = struct
    type t =
      [ `Error
      | `Warning
      | `Info
      | `Success ] 
      [@@deriving sexp]

    let datagram_fmt t =
      let to_string = function
        | `Error -> "error"
        | `Warning -> "warning"
        | `Info -> "info"
        | `Success -> "success"
      in
      Printf.sprintf "|a:%s" (to_string t)
  end

  type t =
    { title : string
    ; text : string
    ; timestamp : int option
    ; hostname : string option
    ; aggregation_key : string option
    ; priority : Priority.t option (** defaults to `Normal *)
    ; source_type_name : string option
    ; alert_type : Alert.t option (** defaults to `Info *)
    ; tags: Tag.t list } 
    [@@deriving sexp]

  (** For datagram format see:
      https://docs.datadoghq.com/developers/dogstatsd/#events-1 *)
  let datagram_fmt t =
    Printf.sprintf "_e{%d,%d}:%s|%s%s%s%s%s%s%s"
      (String.length t.title)
      (String.length t.text)
      t.title
      t.text
      (value_or_empty_str (Printf.sprintf "|d:%d") t.timestamp)
      (value_or_empty_str (Printf.sprintf "|h:%s") t.hostname)
      (value_or_empty_str (Priority.datagram_fmt) t.priority)
      (value_or_empty_str (Printf.sprintf "|s:%s") t.source_type_name)
      (value_or_empty_str (Alert.datagram_fmt) t.alert_type)
      (Tag.datagram_fmt t.tags)
end

module type T = sig
  type 'a _t
  module Metric : sig
    type t = Metric.t
    val t_send : Metric.t -> unit _t
    val send
      : ?tags:Tag.t list
      -> ?sample_rate:float
      -> Metric.Value_type.t
      -> string
      -> unit _t
  end

  module ServiceCheck : sig
    type t = ServiceCheck.t
    val t_send : ServiceCheck.t -> unit _t
    val send
      : ?tags:Tag.t list
      -> ?message:string
      -> ?hostname:string
      -> ?timestamp:int
      -> ServiceCheck.Status.t
      -> string
      -> unit _t
  end

  module Event : sig
    type t = Event.t
    val t_send : Event.t -> unit _t
    val send
      : ?tags:Tag.t list
      -> ?hostname:string
      -> ?timestamp:int
      -> ?aggregation_key:string
      -> ?priority:Event.Priority.t
      -> ?source_type_name:string
      -> ?alert_type:Event.Alert.t
      -> title:string
      -> text:string
      -> unit _t
  end
end

module Make (IO : Statsd_client_core.IO)
  : (T with type 'a _t := 'a IO._r) = struct

  module U = Statsd_client_core.Make(IO)

  module Metric = struct
    type t = Metric.t
    let t_send t = U.send ~data:[Metric.datagram_fmt t]
    let send ?(tags=[]) ?sample_rate metric metric_name =
      t_send { Metric.metric_name ; metric ; sample_rate ; tags }
  end

  module ServiceCheck = struct
    type t = ServiceCheck.t
    let t_send t = U.send ~data:[ServiceCheck.datagram_fmt t]
    let send ?(tags=[]) ?message ?hostname ?timestamp status name =
      t_send { ServiceCheck.name
             ; status
             ; timestamp
             ; hostname
             ; tags
             ; message }
  end

  module Event = struct
    type t = Event.t
    let t_send t = U.send ~data:[Event.datagram_fmt t]
    let send ?(tags=[]) ?hostname ?timestamp ?aggregation_key ?priority
        ?source_type_name ?alert_type ~title ~text =
      t_send { Event.title
             ; text
             ; timestamp
             ; hostname
             ; aggregation_key
             ; priority
             ; source_type_name
             ; alert_type
             ; tags }
  end
end
