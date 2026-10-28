//// Parses the etodo.toml configuration file for the Erlang side.
////
//// Tables become maps with binary keys, strings become binaries and arrays
//// become lists, so `eTodoConfig` can map them to application settings.

import gleam/dynamic.{type Dynamic}
import gleam/string
import tom

pub fn parse(text: String) -> Result(Dynamic, String) {
  case tom.parse_to_dynamic(text) {
    Ok(config) -> Ok(config)
    Error(tom.Unexpected(got:, expected:)) ->
      Error("unexpected " <> string.inspect(got) <> ", expected " <> expected)
    Error(tom.KeyAlreadyInUse(key:)) ->
      Error("the key " <> string.join(key, ".") <> " is set twice")
  }
}
