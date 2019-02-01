open Err

open Ofluxl_std

let make_tracker () = object
  val mutable lines = []

  method add line = 
    lines <- line :: lines

  method finish = 
    List.rev lines
    |> String.concat ~sep:"\n"
end


let counter = object
  val mutable counter = 0
  method next =
    counter <- counter + 1;
    counter
end

type table = 
  { group: string list 
  ; columns: string list
  ; index: int
  }
[@@deriving sexp_of]

let to_set = Set.of_list (module String)
let to_list = Set.to_list

let with_drop table dropped =
  let dropped_set = to_set dropped in
  let column_set = to_set table.columns in
  { group = table.group
  ; columns = Set.diff column_set dropped_set |> to_list
  ; index = counter#next
  }

let with_group {columns; index; _} grouped =
  let group_set = to_set grouped in
  let column_set = to_set columns in
  if not @@ Set.is_subset group_set ~of_:column_set
  then throw @@ InvalidGroup (columns, grouped)
  else { group = grouped
       ; columns
       ; index
       }

let with_join left right on =
  let left_columns = to_set left.columns in
  let right_columns = to_set right.columns in
  let on = match on with
    | [] -> Set.inter left_columns right_columns
    | on -> to_set on
  in
  let left_rem =
    Set.diff left_columns on
    |> Set.map (module String) ~f:(fun col -> col ^ "_left")
  in
  let right_rem = 
    Set.diff right_columns on
    |> Set.map (module String) ~f:(fun col -> col ^ "_right")
  in
  let left_group = 
    List.map left.group ~f:(fun name ->
        if Set.mem on name then name else name ^ "_left")
    |> to_set
  in
  let right_group =
    List.map right.group ~f:(fun name ->
        if Set.mem on name then name else name ^ "_right")
    |> to_set
  in
  { columns = Set.union on (Set.union left_rem right_rem) |> to_list
  ; group = Set.union left_group right_group |> to_list
  ; index = counter#next
  }

let type_name_of {index; _} =
  sprintf "table%d" index

let type_def_of table =
  (* AHH *)
  let fields = 
    table.columns
    |> List.map ~f:(fun col -> sprintf "%s: f64" col)
    |> String.concat ~sep:", "
  in
  sprintf "type %s = {%s}" (type_name_of table) fields


let generate_leq table =
  let tracker = make_tracker () in
  let name = sprintf "leq_%s_%d" (type_name_of table) counter#next in
  tracker#add @@ sprintf "let %s a b =" name;
  List.iter table.group ~f:(fun name ->
      tracker#add @@ sprintf "\tif a.%s < b.%s then true else" name name;
      tracker#add @@ sprintf "\tif a.%s > b.%s then false else" name name);
  tracker#add "\ttrue";
  name, tracker#finish

let generate_equal table =
  let typ = type_name_of table in
  let tracker = make_tracker () in
  let name = sprintf "eq_%s_%d" (type_name_of table) counter#next in
  tracker#add @@ sprintf "let %s (a: %s) (b: %s) =" name typ typ;
  List.iter table.group ~f:(fun name ->
      tracker#add @@ sprintf "\tif a.%s != b.%s then false else" name name);
  tracker#add "\ttrue";
  name, tracker#finish

let generate_table_change left right =
  let tracker = make_tracker () in
  let name = sprintf "map_%s_to_%s_%d" (type_name_of left) (type_name_of right) counter#next in

  let left_group = to_set left.group in
  let right_group = to_set right.group in

  let left_columns = to_set left.columns in
  let right_columns = to_set right.columns in

  if not @@ Set.is_subset right_columns ~of_:left_columns
  then throw @@ InvalidTables (left.columns, right.columns)
  else begin
    tracker#add @@ sprintf "let %s (values: []%s): []%s =" name (type_name_of left) (type_name_of right);

    if not @@ Set.equal left_columns right_columns then
      let fields =
        right.columns
        |> List.map ~f:(fun name -> sprintf "%s = value.%s" name name)
        |> String.concat ~sep:", "
      in
      tracker#add @@ sprintf "\tlet drop (value: %s): %s = {%s} in" (type_name_of left) (type_name_of right) fields;
      tracker#add @@ sprintf "\tlet values = map drop values in"
    else ();

    if not @@ Set.equal left_group right_group then begin
      tracker#add "\tlet le a b =";
      List.iter right.group ~f:(fun name ->
          tracker#add @@ sprintf "\t\tif a.%s < b.%s then true else" name name;
          tracker#add @@ sprintf "\t\tif a.%s > b.%s then false else" name name);
      tracker#add "\t\ttrue";
      tracker#add "\tlet values = merge_sort le values in"
    end else ();

    tracker#add "\tvalues";
    name, tracker#finish
  end

