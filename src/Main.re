module Matchers = (D: Decoders.Decode.S) => {
  /* parameterising the decoders backend allows us to re-use
     the same code in Bucklescript and in native Ocaml. */

  /** Represents an item in the input JSON list */

  type candidate = {
    /** The original JSON item. We'll return this if any of the values match. */
    orig_json: D.value,
    /** Strings extracted from the JSON item we'll match against. */
    strings: list(string),
  };

  /** Given a [key] like ["a.b.c"], decode the string at c.

      This decoder uses [D.maybe], so instead of failing when the JSON does not
      have the right shape, it will succeed with [None].

      To allow array indexing in the key (e.g. ["a.b.0.c"]), you can write a
      customized version of [D.at] that checks whether an item in the path is an
      integer, and then uses [D.index] instead of [D.field].

      See https://github.com/mattjbray/ocaml-decoders/blob/e00bad1d2e4c2b1394aee9ae5a7a6fe3ab04ecec/src/decode.ml#L606
   */

  let decode_string_at_key = (key: string): D.decoder(option(string)) => {
    let path = String.split_on_char('.', key);
    D.maybe(D.at(path, D.string));
  };

  /** Decode all the strings in an object following [keys]. */

  let rec decode_strings = (~keys, ()): D.decoder(list(string)) =>
    D.Infix.(
      switch (keys) {
      | [key, ...keys] =>
        decode_string_at_key(key)
        >>= (
          str =>
            decode_strings(~keys, ())
            >>= (
              strs => {
                let strs =
                  switch (str) {
                  | Some(str) => [str, ...strs]
                  | None => strs
                  };
                D.succeed(strs);
              }
            )
        )
      | [] => D.succeed([])
      }
    );

  let decode_candidate = (~keys=?, ()): D.decoder(candidate) =>
    D.Infix.(
      D.value
      >>= (
        orig_json => {
          let strings =
            switch (keys) {
            | None =>
              /* No keys passed, assume value is a simple string. */
              D.maybe(D.string)
              |> D.map(
                   fun
                   | Some(s) => [s]
                   | None => [],
                 )
            | Some(keys) => decode_strings(~keys, ())
            };

          strings >>= (strings => D.succeed({orig_json, strings}));
        }
      )
    );

  let decode_candidates = (~keys=?, ()): D.decoder(list(candidate)) =>
    D.list(decode_candidate(~keys?, ()));
};

/** Assuming you're using bucklescript - use Decoders_yojson.Basic.Decode or something if you're using native */
module D = Decoders_bs.Decode;
module Matchers_bs = Matchers(D);

let candidates =
    (~keys=?, json: Js.Json.t): result(list(Matchers_bs.candidate), D.error) =>
  D.decode_value(Matchers_bs.decode_candidates(~keys?, ()), json);

type my_user = {
  name: string,
  age: int,
};

let my_user_decoder: D.decoder(my_user) = (
  D.(
    field("name", string)
    >>= (name => field("age", int) >>= (age => succeed({name, age})))
  ):
    D.decoder(my_user)
);
Js.log2("my_user_decoder",D.succeed(my_user_decoder))
//  D.succeed(my_user_decoder)
