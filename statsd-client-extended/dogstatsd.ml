module Base = struct
  module Tag = struct
    type t = string * string (** (tag-name, value) *)

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
          | `Value of int ] (** Decrements or Increments by the given value *)
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

    let sample_rate_datagram_fmt = function
      | None -> ""
      | Some s -> Printf.sprintf "|@%f" s

    let datagram_fmt t =
      (** https://docs.datadoghq.com/developers/dogstatsd/#metrics-1 *)
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
    end
    type t =
      { name : string
      ; status : Status.t
      ; timestamp : int option
      ; hostname : string option
      ; tags : Tag.t list
      ; message: string option }
  end

  (** https://docs.datadoghq.com/developers/dogstatsd/#events *)
  module Event = struct
    type priority = [ `Normal | `Low ]

    type alert =
      [ `Error
      | `Warning
      | `Info
      | `Success ]

    type t =
      { title : string
      ; text : string
      ; timestamp : int option
      ; hostname : string option
      ; aggregation_key : string option
      ; priority : priority option (** defaults to `Normal *)
      ; source_type_name : string option
      ; alert_type : alert option (** defaults to `Info *)
      ; tags: Tag.t list }
  end
end

module type T = sig
  type 'a _t
  module Metric : sig
    val t_send : Base.Metric.t -> unit _t
    val send
      : ?tags:Base.Tag.t list
      -> ?sample_rate:float
      -> Base.Metric.Value_type.t
      -> string
      -> unit _t
  end

  module ServiceCheck : sig
    val t_send : Base.ServiceCheck.t -> unit _t
    val send
      : ?tags:Base.Tag.t list
      -> ?message:string
      -> ?hostname:string
      -> ?timestamp:int
      -> Base.ServiceCheck.Status.t
      -> string
      -> unit _t
  end

  module Event : sig
    val t_send : Base.Event.t -> unit _t
    val send
      : ?tags:Base.Tag.t list
      -> ?hostname:string
      -> ?timestamp:int
      -> ?aggregation_key:string
      -> ?source_type_name:string
      -> ?alert_type:Base.Event.alert
      -> title:string
      -> text:string
      -> unit _t
  end
end

module Make (IO : Statsd_client_core.IO) :
  (T with type 'a _t := 'a IO._r) = struct

  module U = Statsd_client_core.Make(IO)

  module Metric = struct
    let t_send t = U.send ~data:[Base.Metric.datagram_fmt t]
    let send ?(tags=[]) ?sample_rate metric metric_name =
      t_send { Base.Metric.metric_name ; metric ; sample_rate ; tags }
  end

  module ServiceCheck = struct
    let t_send t = t |> ignore; failwith "not yet implemented"
    let send ?tags ?message ?hostname ?timestamp status check =
      (tags, message, hostname, timestamp, status, check) |> ignore;
      failwith "not yet implemented"
  end

  module Event = struct
    let t_send t = t |> ignore; failwith "not yet implemented"
    let send ?tags ?hostname ?timestamp ?aggregation_key ?source_type_name
        ?alert_type ~title ~text =
      (tags, hostname, timestamp, aggregation_key, source_type_name,
       alert_type, title, text) |> ignore;
      failwith "not yet implemented"
  end
end
