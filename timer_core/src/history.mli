(** Saved sets: the record the MVP keeps under {!storage_key} (a JSON array
    of sets, oldest first), its JSON codec, and the pure operations on the
    list (upsert, remove, re-logging the weight, exports, the PowerCurve
    payload).

    Key order on output is the order the MVP creates them in. Sets saved
    before uploads existed lack [source_session_id]; the MVP adds one at the
    end of the object on first upload, so their key order can differ from
    what {!to_json} writes. That changes no value. Unknown keys of sets and
    reps are kept and written back. *)

type performance = { rank : float option; prior_session_count : int }

type pc =
  | Uploading
  | Uploaded of { at : string; result : string; performance : performance option }
      (** [result]: ["created"], ["already_uploaded"] or ["merged"]. *)
  | Failed of { at : string; message : string }
  | Unknown_pc of Json.t  (** Anything else found in storage, kept verbatim. *)

type set = {
  id : string;  (** [Date.now().toString(36)] at start. *)
  source_session_id : string option;  (** A UUID; [None] for old sets. *)
  date_time : string;  (** {!Jsnum.iso_utc} at start. *)
  gripper : string;
  side : string;
  target_kg : float;
  planned_reps : int;
  countdown_s : float;
  rest_s : float;
  rep_end_after_pct : float;  (** The engage %. *)
  rep_end_below_pct : float;  (** The disengage %. *)
  tolerance_pct : float;
  reps : Rep.t list;  (** In order. *)
  pc : pc option;  (** PowerCurve upload state, absent until the first upload. *)
  extra : (string * Json.t) list;
}

val storage_key : string
(** ["grip-gains-force.history.v1"]. *)

val pc_to_json : pc -> Json.t
(** The upload state as a set's ["pc"] member holds it. *)

val pc_of_json : Json.t -> pc
(** Anything unrecognised reads as {!Unknown_pc}. *)

val set_to_json : set -> Json.t
val set_of_json : Json.t -> (set, string) result
(** [id] is required; other fields fall back to empty/zero values. *)

val of_json : Json.t -> set list
(** A non-array reads as [[]] (as the MVP). Entries that are not readable
    sets are dropped. *)

val of_string : string option -> set list
val to_json : set list -> Json.t
val to_string : set list -> string
(** [JSON.stringify(history)]. *)

(** {2 List operations} *)

val upsert : set list -> set -> set list
(** [saveCurrentSet]: replace the set with the same id in place, else
    append. *)

val remove : set list -> string list -> set list
(** Delete the sets with these ids. *)

val relog : set -> float -> set
(** [saveReview] with a corrected weight (kg): if it differs from
    [target_kg] by more than 1e-6, set [target_kg] and recompute each rep's
    [dev_kg = r2(median_kg − kg)]; otherwise unchanged. *)

(** {2 Exports} *)

val weight_lbs : set -> float
(** [+(target_kg · 2.20462).toFixed(1)]. *)

val rep_seconds : Rep.t -> int
(** Whole seconds, floored (a stopwatch readout). *)

val export_json : set list -> string
(** "Copy JSON": one set as an object, several as an array, each
    [{date_time, gripper, side, weight_lbs, reps: [whole seconds]}],
    indented by 2 as [JSON.stringify(_, null, 2)]. *)

val pc_url : string
val pc_helper_version : string
val pc_token_key : string
(** localStorage key of the upload token. *)

val valid_pc_token : string -> bool
(** [pcgg_] followed by 64 lowercase hex digits. *)

val pc_payload : set -> session_id:string -> Json.t
(** The upload body. [session_id] is used only when the set has none (the
    MVP generates one and stores it on the set first: do the same with
    {!with_session_id}). Rep seconds are floored and at least 1. *)

val with_session_id : set -> string -> set
(** Give an old set a [source_session_id] if it lacks one. *)

val pc_result : status:int option -> body:Json.t -> at:string -> pc
(** How [uploadSet] turns the HTTP reply into the stored state: [status =
    None] when the request failed; [body] is the parsed reply ([Object []]
    if it was not JSON). Any [body] is accepted: [null], an array or a
    string reads like an object without the expected fields (fix 7: the
    MVP threw on a [null] reply and left the set "uploading" for good). *)

val pc_shown : pc option -> in_flight:bool -> pc option
(** The upload state to show and act on. [Uploading] only while this page's
    request for the set is in flight; an [Uploading] left in storage (the
    page closed mid-upload, or the old version's history) reads as a failed
    upload that can be retried, never as uploading for ever (fix 7). *)

val interrupted_message : string

val rank_text : performance option -> set -> string
(** The rank line of the set sheet. *)
