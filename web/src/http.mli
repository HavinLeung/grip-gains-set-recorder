(** The one HTTP client: a request, and its answer as a status and a JSON body.
    Every call the app makes (its own API, PowerCurve) goes through {!send}. *)

type response = {
  status : int;
  json : Js.Json.t option;  (** [None]: the body wasn't JSON. *)
}

val send :
  ?timeout_ms:float ->
  ?credentials:[ `Same_origin | `Omit ] ->
  ?headers:(string * string) list ->
  ?body:string ->
  string ->
  string ->
  (response, Browser.err) result Js.Promise.t
(** [send meth url]. Never rejects: [Error] is a request that got no answer (no
    connection, blocked, timed out after [timeout_ms]; no limit by default).

    [body] is sent as it is, as [application/json] unless [headers] say
    otherwise. [credentials] defaults to [`Same_origin] (the session cookie goes
    to this site only). *)
