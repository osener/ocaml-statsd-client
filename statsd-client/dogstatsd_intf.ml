(** https://docs.datadoghq.com/developers/dogstatsd/ *)
module type S = sig
  type 'a _t

  module Tag : sig
    type t = string * string (** (name, value) *) 
    [@@deriving sexp] 
  end

  module Metric : sig
    (** A couple of examples of how to send metrics:

        * Async.Metric.send
            ~tags:["tag1", "val1"; "tag2","val2"]
            ~sample_rate:0.1
            (`Counter `Increment) "application.page_views"
        * Async.Metric.send
            (`Set "some-user-id") "application.user_ids"
    *)

    module Value_type : sig
      module Counter : sig
        type t =
          [ `Increment (** Increments by 1 *)
          | `Decrement (** Decrements by 1 *)
          | `Value of int ] 
        [@@deriving sexp]  
        (** Decrements or Increments by the given value *)
      end

      type t =
        [ `Counter of Counter.t
        | `Gauge of float
        | `Timer of float
        | `Histogram of int
        | `Set of int ]
      [@@deriving sexp] 
    end

    type t = 
      { metric_name : string
      ; metric : Value_type.t
      ; sample_rate : float option
      (** Sample rates only work with `Counter, `Histogram and `Timer typ
          metrics *)
      ; tags : Tag.t list } 
    [@@deriving sexp] 

    val t_send : t -> unit _t

    val send
      : ?tags:Tag.t list
      -> ?sample_rate:float
      -> Value_type.t
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

    module Status : sig
      type t =
        [ `Ok
        | `Warning
        | `Critical
        | `Unknown ] 
      [@@deriving sexp] 
    end

    type t =   
      { name : string
      ; status : Status.t
      ; timestamp : int option
      ; hostname : string option
      ; tags : Tag.t list
      ; message: string option } 
    [@@deriving sexp] 

    val t_send : t -> unit _t

    val send
      : ?tags:Tag.t list
      -> ?message:string
      -> ?hostname:string
      -> ?timestamp:int
      -> Status.t
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

    module Priority : sig
      type t = [ `Normal | `Low ]
      [@@deriving sexp] 
    end

    module Alert : sig
      type t =
        [ `Error
        | `Warning
        | `Info
        | `Success ] 
      [@@deriving sexp] 
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

    val t_send : t -> unit _t

    val send
      : ?tags:Tag.t list
      -> ?hostname:string
      -> ?timestamp:int
      -> ?aggregation_key:string
      -> ?priority:Priority.t
      -> ?source_type_name:string
      -> ?alert_type:Alert.t
      -> title:string
      -> text:string
      -> unit _t
  end
end