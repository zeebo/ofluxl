output = table
    |> drop(columns: ["a", "c"])
    |> filter(fn: (r) => (r.b < 1.05) and r.b > 0.95)
    |> group(columns: ["d"])
    |> sum(columns: ["b"]);
