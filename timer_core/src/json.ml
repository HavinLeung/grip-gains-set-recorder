type t = Null | Bool of bool | Number of float | String of string | Array of t list | Object of (string * t) list

let is_js = match Sys.backend_type with Native | Bytecode -> false | Other _ -> true

exception Fail of string

let fail i msg = raise (Fail ("JSON: " ^ msg ^ " at " ^ string_of_int i))

let add_utf8 b cp =
  let add c = Buffer.add_char b (Char.unsafe_chr c) in
  if cp < 0x80 then add cp
  else if cp < 0x800 then begin
    add (0xC0 lor (cp lsr 6));
    add (0x80 lor (cp land 0x3F))
  end
  else if cp < 0x10000 then begin
    add (0xE0 lor (cp lsr 12));
    add (0x80 lor ((cp lsr 6) land 0x3F));
    add (0x80 lor (cp land 0x3F))
  end
  else begin
    add (0xF0 lor (cp lsr 18));
    add (0x80 lor ((cp lsr 12) land 0x3F));
    add (0x80 lor ((cp lsr 6) land 0x3F));
    add (0x80 lor (cp land 0x3F))
  end

(* Melange's String.sub copies the whole source string first (O(length of s)),
   which makes parsing a large document quadratic; copy just the slice. *)
let slice s start len = String.init len (fun i -> String.unsafe_get s (start + i))

let parse s =
  let n = String.length s in
  let pos = ref 0 in
  let peek () = if !pos < n then Some s.[!pos] else None in
  let rec ws () =
    match peek () with
    | Some (' ' | '\t' | '\n' | '\r') ->
        incr pos;
        ws ()
    | _ -> ()
  in
  let expect c =
    if !pos < n && s.[!pos] = c then incr pos else fail !pos ("expected '" ^ String.make 1 c ^ "'")
  in
  let literal word v =
    let l = String.length word in
    if !pos + l <= n && slice s !pos l = word then begin
      pos := !pos + l;
      v
    end
    else fail !pos "unexpected token"
  in
  let hex4 () =
    if !pos + 4 > n then fail !pos "bad \\u escape";
    let v = ref 0 in
    for k = 0 to 3 do
      let c = Char.code s.[!pos + k] in
      let d =
        if c >= 48 && c <= 57 then c - 48
        else if c >= 97 && c <= 102 then c - 87
        else if c >= 65 && c <= 70 then c - 55
        else fail (!pos + k) "bad hex digit"
      in
      v := (!v * 16) + d
    done;
    pos := !pos + 4;
    !v
  in
  let string () =
    expect '"';
    let b = Buffer.create 16 in
    let rec go () =
      if !pos >= n then fail !pos "unterminated string";
      let start = !pos in
      while !pos < n && s.[!pos] <> '"' && s.[!pos] <> '\\' && Char.code s.[!pos] >= 0x20 do
        incr pos
      done;
      (* Not Buffer.add_substring: under Melange it also copies all of [s]. *)
      if !pos > start then Buffer.add_string b (slice s start (!pos - start));
      if !pos >= n then fail !pos "unterminated string";
      match s.[!pos] with
      | '"' -> incr pos
      | '\\' ->
          incr pos;
          if !pos >= n then fail !pos "bad escape";
          let c = s.[!pos] in
          incr pos;
          (match c with
          | '"' -> Buffer.add_char b '"'
          | '\\' -> Buffer.add_char b '\\'
          | '/' -> Buffer.add_char b '/'
          | 'b' -> Buffer.add_char b '\b'
          | 'f' -> Buffer.add_char b '\012'
          | 'n' -> Buffer.add_char b '\n'
          | 'r' -> Buffer.add_char b '\r'
          | 't' -> Buffer.add_char b '\t'
          | 'u' ->
              let u = hex4 () in
              if is_js then Buffer.add_char b (Char.unsafe_chr u)
              else if u >= 0xD800 && u <= 0xDBFF && !pos + 6 <= n && s.[!pos] = '\\' && s.[!pos + 1] = 'u'
              then begin
                let save = !pos in
                pos := !pos + 2;
                let lo = hex4 () in
                if lo >= 0xDC00 && lo <= 0xDFFF then add_utf8 b (0x10000 + ((u - 0xD800) lsl 10) + (lo - 0xDC00))
                else begin
                  pos := save;
                  add_utf8 b u
                end
              end
              else add_utf8 b u
          | _ -> fail (!pos - 1) "bad escape");
          go ()
      | _ -> fail !pos "control character in string"
    in
    go ();
    Buffer.contents b
  in
  let number () =
    let start = !pos in
    let digits () =
      let d0 = !pos in
      while !pos < n && s.[!pos] >= '0' && s.[!pos] <= '9' do
        incr pos
      done;
      if !pos = d0 then fail !pos "expected digit"
    in
    if peek () = Some '-' then incr pos;
    (match peek () with
    | Some '0' -> incr pos
    | Some ('1' .. '9') -> digits ()
    | _ -> fail !pos "expected digit");
    if peek () = Some '.' then begin
      incr pos;
      digits ()
    end;
    (match peek () with
    | Some ('e' | 'E') ->
        incr pos;
        (match peek () with Some ('+' | '-') -> incr pos | _ -> ());
        digits ()
    | _ -> ());
    Number (float_of_string (slice s start (!pos - start)))
  in
  let rec value () =
    ws ();
    match peek () with
    | Some '{' ->
        incr pos;
        ws ();
        if peek () = Some '}' then begin
          incr pos;
          Object []
        end
        else
          let rec members acc =
            ws ();
            let k = string () in
            ws ();
            expect ':';
            let v = value () in
            ws ();
            match peek () with
            | Some ',' ->
                incr pos;
                members ((k, v) :: acc)
            | Some '}' ->
                incr pos;
                Object (List.rev ((k, v) :: acc))
            | _ -> fail !pos "expected ',' or '}'"
          in
          members []
    | Some '[' ->
        incr pos;
        ws ();
        if peek () = Some ']' then begin
          incr pos;
          Array []
        end
        else
          let rec items acc =
            let v = value () in
            ws ();
            match peek () with
            | Some ',' ->
                incr pos;
                items (v :: acc)
            | Some ']' ->
                incr pos;
                Array (List.rev (v :: acc))
            | _ -> fail !pos "expected ',' or ']'"
          in
          items []
    | Some '"' -> String (string ())
    | Some 't' -> literal "true" (Bool true)
    | Some 'f' -> literal "false" (Bool false)
    | Some 'n' -> literal "null" Null
    | Some ('-' | '0' .. '9') -> number ()
    | _ -> fail !pos "unexpected token"
  in
  match
    let v = value () in
    ws ();
    if !pos <> n then fail !pos "trailing characters";
    v
  with
  | v -> Ok v
  | exception Fail m -> Error m

let hex = "0123456789abcdef"

let escape_u b c =
  Buffer.add_string b "\\u";
  Buffer.add_char b hex.[(c lsr 12) land 15];
  Buffer.add_char b hex.[(c lsr 8) land 15];
  Buffer.add_char b hex.[(c lsr 4) land 15];
  Buffer.add_char b hex.[c land 15]

let add_quoted b s =
  Buffer.add_char b '"';
  let n = String.length s in
  let i = ref 0 in
  while !i < n do
    let ch = s.[!i] in
    let c = Char.code ch in
    (match ch with
    | '"' -> Buffer.add_string b "\\\""
    | '\\' -> Buffer.add_string b "\\\\"
    | '\b' -> Buffer.add_string b "\\b"
    | '\012' -> Buffer.add_string b "\\f"
    | '\n' -> Buffer.add_string b "\\n"
    | '\r' -> Buffer.add_string b "\\r"
    | '\t' -> Buffer.add_string b "\\t"
    | _ when c < 0x20 -> escape_u b c
    | _ when c >= 0xD800 && c <= 0xDBFF ->
        (* Only reachable under Melange, where chars are UTF-16 code units. *)
        let next = if !i + 1 < n then Char.code s.[!i + 1] else 0 in
        if next >= 0xDC00 && next <= 0xDFFF then begin
          Buffer.add_char b ch;
          Buffer.add_char b s.[!i + 1];
          incr i
        end
        else escape_u b c
    | _ when c >= 0xDC00 && c <= 0xDFFF -> escape_u b c
    | _ -> Buffer.add_char b ch);
    incr i
  done;
  Buffer.add_char b '"'

let to_string ?(indent = 0) v =
  let b = Buffer.create 256 in
  let nl depth =
    if indent > 0 then begin
      Buffer.add_char b '\n';
      Buffer.add_string b (String.make (indent * depth) ' ')
    end
  in
  let rec go depth = function
    | Null -> Buffer.add_string b "null"
    | Bool x -> Buffer.add_string b (if x then "true" else "false")
    | Number f -> Buffer.add_string b (if Float.is_finite f then Jsnum.to_string f else "null")
    | String s -> add_quoted b s
    | Array [] -> Buffer.add_string b "[]"
    | Object [] -> Buffer.add_string b "{}"
    | Array l ->
        Buffer.add_char b '[';
        List.iteri
          (fun i x ->
            if i > 0 then Buffer.add_char b ',';
            nl (depth + 1);
            go (depth + 1) x)
          l;
        nl depth;
        Buffer.add_char b ']'
    | Object l ->
        Buffer.add_char b '{';
        List.iteri
          (fun i (k, x) ->
            if i > 0 then Buffer.add_char b ',';
            nl (depth + 1);
            add_quoted b k;
            Buffer.add_string b (if indent > 0 then ": " else ":");
            go (depth + 1) x)
          l;
        nl depth;
        Buffer.add_char b '}'
  in
  go 0 v;
  Buffer.contents b

let member k = function
  | Object l -> List.fold_left (fun acc (k', v) -> if k' = k then Some v else acc) None l
  | _ -> None

let to_number = function Number f -> Some f | _ -> None
let to_bool = function Bool b -> Some b | _ -> None
let to_string_opt = function String s -> Some s | _ -> None
let to_list = function Array l -> Some l | _ -> None

let rec equal a b =
  match (a, b) with
  | Null, Null -> true
  | Bool x, Bool y -> x = y
  | Number x, Number y -> x = y
  | String x, String y -> String.equal x y
  | Array x, Array y -> List.length x = List.length y && List.for_all2 equal x y
  | Object x, Object y ->
      List.length x = List.length y && List.for_all2 (fun (k, v) (k', v') -> String.equal k k' && equal v v') x y
  | _ -> false
