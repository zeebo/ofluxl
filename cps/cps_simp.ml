open Ofluxl_std

open Cps

exception Unimplemented

let rec simp census env subst =
  function
  (* Dead-Val *)
  | LetVal (x, v, l) ->
    if Census.count census = 0
    then simp census env subst l
    else LetVal (x, simpVal census env subst v,
                 simp census (Env.set env x v) subst l)

  (* B-Tuple *)
  | LetPi (x, n, y, l) ->
    let y' = Subst.apply subst y in
    begin match Env.lookup env y' with
      | Some (Tuple vs) ->
        begin match List.nth vs n with
          | Some z ->
            simp census env (Subst.extend subst x z) l
          | None ->
            LetPi (x, n, y', simp census env subst l)
        end
      | _ ->
        LetPi (x, n, y', simp census env subst l)
    end

  | _ -> raise Unimplemented

and simpVal _census _env _subst =
  function
  | _ -> raise Unimplemented