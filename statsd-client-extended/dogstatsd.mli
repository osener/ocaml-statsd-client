module Base : sig
  module Tag : sig
    type t = string * string (** (tag-name, value) *)
  end

  (** https://docs.datadoghq.com/developers/dogstatsd/#metrics *)
  module Metric : sig

    module Value_type : sig
      module Counter : sig
        type t =
          [ `Increment (** Increments by 1 *)
          | `Decrement (** Decrements by 1 *)
          | `Value of int ] (** Decrements or Increments by the given value *)
      end

      type t =
        [ `Counter of Counter.t
        | `Gauge of float
        | `Timer of float
        | `Histogram of int
        | `Set of int ]
    end

    type t =
      { metric_name : string
      ; metric : Value_type.t
      ; sample_rate : float option
      (** Sample rates only work with `Counter, `Histogram and `Timer typ
          metrics *)
      ; tags : Tag.t list }
  end

  (** https://docs.datadoghq.com/developers/dogstatsd/#service-checks *)
  module ServiceCheck : sig
    module Status : sig
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
  module Event : sig
    module Priority : sig
      type t = [ `Normal | `Low ]
    end

    module Alert : sig
      type t =
        [ `Error
        | `Warning
        | `Info
        | `Success ]
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
  end
end

module type T = sig
  type 'a _t

  module Metric : sig
    (** A couple of examples of how to send metrics:

        * Async.Metric.send
            ~tags:["tag1", "val1"; "tag2","val2"]
            ~sample_rate:0.1
            (`Counter `Increment) ["application.page_views"]
        * Async.Metric.send
            (`Set "some-user-id") ["application.user_ids"]
    *)

    val t_send : Base.Metric.t -> unit _t
    val send
      : ?tags:Base.Tag.t list
      -> ?sample_rate:float
      -> Base.Metric.Value_type.t
      -> string
      -> unit _t
  end

  module ServiceCheck : sig
    (** An example of how to send Service Checks:

        ```
        Async.ServiceCheck.send
          ~tags:["tag1", "val1"; "tag2","val2"]
          ~message:"Redis connection timed out after 10s"
          `Critical
          "mywebsite.can_connect_redis"
        ```
    *)

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
    (** An example of how to send Events:

        ```
        Async.Event.send
          ~tags:["tag1", "val1"; "tag2","val2"]
          ~alert_type:`Error
          ~priority:`Low
          ~title:"Page render error!"
          ~text:"Some error message"
        ```
    *)
    val t_send : Base.Event.t -> unit _t
    val send
      : ?tags:Base.Tag.t list
      -> ?hostname:string
      -> ?timestamp:int
      -> ?aggregation_key:string
      -> ?priority:Base.Event.Priority.t
      -> ?source_type_name:string
      -> ?alert_type:Base.Event.Alert.t
      -> title:string
      -> text:string
      -> unit _t
  end
end

module Make : functor (IO: Statsd_client_core.IO)
  -> (T with type 'a _t := 'a IO._r)
