type response = { status : int; json : Js.Json.t option }
type fetch_response
type abort_signal

external abort_timeout : float -> abort_signal = "timeout"
[@@mel.scope "AbortSignal"]

(* Melange records compile to plain JS objects, so this is fetch's RequestInit dictionary;
   [None] fields compile to [undefined], which fetch ignores. *)
type init = {
  method_ : string; [@mel.as "method"]
  headers : string Js.Dict.t;
  body : string option;
  credentials : string;
  signal : abort_signal option;
}

external fetch : string -> init -> fetch_response Js.Promise.t = "fetch"
external status : fetch_response -> int = "status" [@@mel.get]
external text : fetch_response -> string Js.Promise.t = "text" [@@mel.send]

let ( let* ) p f = Js.Promise.then_ f p

let send ?timeout_ms ?(credentials = `Same_origin) ?(headers = []) ?body meth
    url =
  let headers =
    if Option.is_some body && not (List.mem_assoc "Content-Type" headers) then
      ("Content-Type", "application/json") :: headers
    else headers
  in
  let init =
    {
      method_ = meth;
      headers = Js.Dict.fromList headers;
      body;
      credentials =
        (match credentials with
        | `Same_origin -> "same-origin"
        | `Omit -> "omit");
      (* AbortSignal.timeout is missing in older browsers: then there is no limit. *)
      signal =
        Option.bind timeout_ms (fun ms ->
            try Some (abort_timeout ms) with _ -> None);
    }
  in
  let* answer = Browser.attempt_call (fun () -> fetch url init) in
  match answer with
  | Error e -> Js.Promise.resolve (Error e)
  | Ok res ->
      let* body = Browser.attempt (text res) in
      let json =
        match body with
        | Ok t -> ( try Some (Js.Json.parseExn t) with _ -> None)
        | Error _ -> None
      in
      Js.Promise.resolve (Ok { status = status res; json })
