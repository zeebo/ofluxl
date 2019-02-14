open Ofluxl_std

type t =
  | Void
  | Integer of string
  | Float of string
  | Duration of string
  | Time of string
  | Regex of string
  | Char of char
  | Bool of bool
  | String of string

let to_string = function
  | Void -> "()"
  | Integer x
  | Float x
  | Duration x
  | Time x -> x
  | Regex re -> sprintf "/%s/" re
  | Char c -> sprintf "'%c'" c
  | Bool b -> sprintf "%b" b

  (* TODO(jeff): actual escaping *)
  | String s -> sprintf "\"%s\"" s