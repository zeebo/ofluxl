
import "lib/github.com/diku-dk/sorts/merge_sort"
import "stdlib"

type table2 = {b: f64, d: f64}
type table1 = {a: f64, b: f64, c: f64, d: f64}
let agg_table2_6 (rs: []table2): table2 = 
	let combined = map (\r -> r.b) rs |> f64.sum in
	rs[0] with b = combined
let eq_table2_5 (a: table2) (b: table2) =
	if a.d != b.d then false else
	true
let map_table2_to_table2_4 (values: []table2): []table2 =
	let le a b =
		if a.d < b.d then true else
		if a.d > b.d then false else
		true
	let values = merge_sort le values in
	values
let map_table1_to_table2_3 (values: []table1): []table2 =
	let drop (value: table1): table2 = {b = value.b, d = value.d} in
	let values = map drop values in
	values

let main a b c d = unsafe
  let table = map4 (\a b c d -> {a, b, c, d}) a b c d in

let output = ((((((((table) |> (map_table1_to_table2_3))) |> ((filter) (\r ->  (((((r).b) < (1.05))) && ((((r).b) > (0.95)))))))) |> (map_table2_to_table2_4))) |> (stdlib_agg agg_table2_6 eq_table2_5)) in


(map (\r -> r.b) output, map (\r -> r.d) output)

