(** The Tindeq Progressor BLE protocol (from [ProgressorService.swift] via
    the MVP's [onPacket], [liPercent] and [CMD]).

    {2 Bytes}

    Packets are read through a {!view}: a length and a byte getter. The UI
    builds one straight from the notification's [DataView]
    ([{ length = dv.byteLength; get_u8 = fun i -> dv.getUint8(i) }]) without
    copying; {!view_of_array} and {!view_of_string} serve tests and native
    code. Multi-byte fields are decoded here by arithmetic (little-endian
    float32 and uint32), so nothing depends on [Int32]/[Bytes] primitives
    that differ between native OCaml and Melange. Timestamps and millivolts
    are uint32 values and are returned as floats: exact, and safe under
    Melange's 32-bit signed [int]. *)

val service_uuid : string
val notify_uuid : string
val write_uuid : string

type command = Tare | Start_measuring | Shutdown | Battery

val command_byte : command -> int
(** [Tare] 0x64, [Start_measuring] 0x65, [Shutdown] 0x6E, [Battery] 0x6F.
    Each command is written as a single byte. *)

val connect_commands : command list
(** What the MVP sends once notifications are on: [Battery] (its failure is
    logged and ignored; the battery reply is expected next, see
    [awaiting_battery] in {!parse}), then [Start_measuring]. *)

type view = { length : int; get_u8 : int -> int }

val view_of_array : int array -> view
val view_of_string : string -> view

type sample = { kg : float; device_us : float  (** Device clock, µs, uint32. *) }

type packet =
  | Battery_mv of float  (** Battery reply: uint32 millivolts. *)
  | Samples of sample array  (** A weight packet (may be empty). *)
  | Ignored  (** Too short, or not a weight packet. *)

val parse : awaiting_battery:bool -> view -> packet
(** While a battery reply is awaited, a packet of at least 6 bytes starting
    [0x00 0x04] is read as one (millivolts at offset 2). Otherwise a packet of
    at least 6 bytes starting [0x01] is a weight packet: 8-byte
    [(float32 kg, uint32 µs)] pairs from offset 2 for as long as whole pairs
    fit (byte 1, the payload length, is not checked). Anything else is
    ignored. *)

val float32_le : view -> int -> float
val uint32_le : view -> int -> float

val li_curve : (float * int) array
(** Lithium cell (mV, %) points, high to low: 4200→100 … 3300→0. *)

val battery_percent : float -> int
(** Piecewise-linear over {!li_curve}, rounded ([Math.round]); 100 at or
    above 4200 mV, 0 below 3300 mV. *)
