import "lib/github.com/diku-dk/sorts/merge_sort"

let stdlib_sum acc values =
    f64.sum (map acc values)

let stdlib_mean acc values =
    let s = f64.sum (map acc values) in
    let n = length values in
    s / r64 n

let stdlib_variance acc values =
    let len = r64 (length values) in
    let sum = stdlib_sum acc values in
    let avg = sum / len in

    let second = f64.sum (map (\v ->
        let shifted = (acc v) - avg in
        shifted * shifted
    ) values) in

    second / (len - 1)

let stdlib_stddev acc values =
    f64.sqrt (stdlib_variance acc values)

let stdlib_skew acc values =
    let len = r64 (length values) in
    let sum = stdlib_sum acc values in
    let avg = sum / len in

    let second = f64.sum (map (\v ->
        let shifted = (acc v) - avg in
        shifted * shifted
    ) values) in

    let third = f64.sum (map (\v ->
        let shifted = (acc v) - avg in
        shifted * shifted * shifted
    ) values) in

    let std = f64.sqrt second in

    (f64.sqrt len) * third / (std * std * std)

let stdlib_kurtosis acc values =
    let len = r64 (length values) in
    let sum = stdlib_sum acc values in
    let avg = sum / len in

    let second = f64.sum (map (\v ->
        let shifted = (acc v) - avg in
        shifted * shifted
    ) values) in

    let fourth = f64.sum (map (\v ->
        let shifted = (acc v) - avg in
        shifted * shifted * shifted * shifted
    ) values) in

    len * fourth / (second * second)

let stdlib_product join a b =
    let op i j = unsafe (join a[i] b[j]) in
    flatten (tabulate_2d (length a) (length b) op)

let stdlib_groups 't (eq: t -> t -> bool) (values: []t): []i32 =
    iota (length values)
    |> map (\i -> if i == 0 || (eq values[i-1] values[i]) then 0 else 1)
    |> scan (+) 0i32

let enumerate values =
    zip (iota (length values)) values

let stdlib_agg 't (agg: []t -> t) (eq: t -> t -> bool) (values: []t): []t =
    let groups = stdlib_groups eq values in
    let select group =
        let _ = trace group in
        values
        |> enumerate
        |> filter (\(i, _) -> groups[i] == group)
        |> map (\(_, v) -> v)
    in
    let _ = trace groups in
    map (\i -> agg (select i)) (iota (last groups + 1))
