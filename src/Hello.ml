module Matchers (D : Decoders.Decode.S)
  (* parameterising the decoders backend allows us to re-use
     the same code in Bucklescript and in native Ocaml. *)
  = struct
  (** Represents an item in the input JSON list *)
  type candidate =
    { orig_json : D.value
          (** The original JSON item. We'll return this if any of the values match. *)
    ; strings : string list
          (** Strings extracted from the JSON item we'll match against. *)
    }

  (** Given a [key] like ["a.b.c"], decode the string at c.

      This decoder uses [D.maybe], so instead of failing when the JSON does not
      have the right shape, it will succeed with [None].

      To allow array indexing in the key (e.g. ["a.b.0.c"]), you can write a
      customized version of [D.at] that checks whether an item in the path is an
      integer, and then uses [D.index] instead of [D.field].

      See https://github.com/mattjbray/ocaml-decoders/blob/e00bad1d2e4c2b1394aee9ae5a7a6fe3ab04ecec/src/decode.ml#L606
   *)
  let decode_string_at_key (key : string) : string option D.decoder =
    let path = String.split_on_char '.' key in
    D.maybe (D.at path D.string)


  (** Decode all the strings in an object following [keys]. *)
  let rec decode_strings ~keys () : string list D.decoder =
    let open D.Infix in
    match keys with
    | key :: keys ->
         decode_string_at_key key >>= fun str ->
         decode_strings ~keys () >>= fun strs ->
        let strs = match str with Some str -> str :: strs | None -> strs in
        D.succeed strs
    | [] ->
        D.succeed []


  let decode_candidate ?keys () : candidate D.decoder =
    let open D.Infix in
    D.value >>= fun orig_json ->
    let strings =
      match keys with
      | None ->
          (* No keys passed, assume value is a simple string. *)
          D.maybe D.string |> D.map (function | Some s -> [ s ] | None -> [])
      | Some keys ->
          decode_strings ~keys ()
    in
    strings >>= fun strings ->
    D.succeed { orig_json; strings }


  let decode_candidates ?keys () : candidate list D.decoder =
    D.list (decode_candidate ?keys ())
end

(** Assuming you're using bucklescript - use Decoders_yojson.Basic.Decode or something if you're using native *)
module D = Decoders_bs.Decode
module Matchers_bs = Matchers (D)

let candidates ?keys (json : Js.Json.t) : (Matchers_bs.candidate list, D.error) result =
  D.decode_value (Matchers_bs.decode_candidates ?keys ()) json



