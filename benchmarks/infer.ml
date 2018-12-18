open Ofluxl

let infer str () =
  try
    Syntax.Parse.str_exn str
    |> Infer.Solve.solve_exn
    |> ignore
  with | _ -> ()

let benchmarks = [
  "from", infer {|
    from
  |};

  "comment", infer {|
    // Comment
    from()
  |};

  "identifier with number", infer {|
    tan2()
  |};

  "regex literal", infer {|
    /.*/
  |};

  "regex literal with escape sequence", infer {|
    /a\/b\\c\d/
  |};

  "regex match operators", infer {|
    "a" =~ /.*/ and "b" !~ /c$/
  |};

  "declare variable as an int", infer {|
    howdy = 1
  |};

  "declare variable as a float", infer {|
    howdy = 1.1
  |};

  "declare variable as an array", infer {|
    howdy = [1, 2, 3, 4]
  |};

  "use variable to declare something", infer {|
    howdy = 1
    from()
  |};

  "variable is from statement", infer {|
    howdy = from()
    howdy.count()
  |};

  "pipe expression", infer {|
    from() |> count()
  |};

  "literal pipe expression", infer {|
    5 |> pow2()
  |};

  "member expression pipe expression", infer {|
    foo.bar |> baz()
  |};

  "multiple pipe expressions", infer {|
    from() |> range() |> filter() |> count()
  |};

  "two variables for two froms", infer {|
    howdy = from()
    doody = from()
    howdy|>count()
    doody|>sum()
  |};

  "from with database", infer {|
    from(bucket:"telegraf/autogen")
  |};

  "map member expressions", infer {|
    m = {key1: 1, key2:"value2"}
    m.key1
    m["key2"]
  |};

  "index expression", infer {|
    a[3]
  |};

  "nested index expression", infer {|
    a[3][5]
  |};

  "access indexed object returned from function call", infer {|
    f()[3]
  |};

  "index with member expressions", infer {|
    a.b["c"]
  |};

  "index with member with call expression", infer {|
    a.b()["c"]
  |};

  "var as binary expression of other vars", infer {|
    a = 1
    b = 2
    c = a + b
    d = a
  |};

  "var as unary expression of other vars", infer {|
    a = 5
    c = -a
  |};

  "var as both binary and unary expressions", infer {|
    a = 5
    c = 10 * -a
  |};

  "unary expressions within logical expression", infer {|
    a = 5.0
    10.0 * -a == -0.5 or a == 6.0
  |};

  "unary expressions with too many comments", infer {|
    // define a
    a = 5.0
    // eval this
    10.0 * -a == -0.5
    // or this
    or a == 6.0
  |};

  "expressions with function calls", infer {|
    a = foo() == 10
  |};

  "mix unary logical and binary expressions", infer {|
    not (f() == 6.0 * x) or fail()
  |};

  "mix unary logical and binary expressions with extra parens", infer {|
    (not (f() == 6.0 * x) or fail())
  |};

  "arrow function called", infer {|
    plusOne = (r) => r + 1
    plusOne(r:5)
  |};

  "arrow function return map", infer {|
    toMap = (r) =>({r:r})
  |};

  "arrow function with default arg", infer {|
    addN = (r, n=5) => r + n
  |};

  "arrow function called in binary expression", infer {|
    plusOne = (r) => r + 1
    plusOne(r:5) == 6 or die()
  |};

  "arrow function as single expression", infer {|
    f = (r) => r["_measurement"] == "cpu"
  |};

  "arrow function as block", infer {|
    f = (r) => { 
      m = r["_measurement"]
      return m == "cpu"
    }
  |};

  "from with filter with no parens", infer {|
    from(bucket:"telegraf/autogen").filter(fn: (r) => r["other"]=="mem" and r["this"]=="that" or r["these"]!="those")
  |};

  "from with range", infer {|
    from(bucket:"telegraf/autogen")|>range(start:-1h, end:10m)
  |};

  "from with limit", infer {|
    from(bucket:"telegraf/autogen")|>limit(limit:100, offset:10)
  |};

  "from with range and count", infer {|
    from(bucket:"mydb/autogen")
    |> range(start:-4h, stop:-2h)
    |> count()
  |};

  "from with range, limit and count", infer {|
    from(bucket:"mydb/autogen")
    |> range(start:-4h, stop:-2h)
    |> limit(n:10)
    |> count()
  |};

  "from with join", infer {|
    a = from(bucket:"dbA/autogen") |> range(start:-1h)
    b = from(bucket:"dbB/autogen") |> range(start:-1h)
    join(tables:[a,b], on:["host"], fn: (a,b) => a["_field"] + b["_field"])
  |};

  "from with join with complex expression", infer {|
    a = from(bucket:"Flux/autogen")
    |> filter(fn: (r) => r["_measurement"] == "a")
    |> range(start:-1h)

    b = from(bucket:"Flux/autogen")
    |> filter(fn: (r) => r["_measurement"] == "b")
    |> range(start:-1h)

    join(tables:[a,b], on:["t1"], fn: (a,b) => (a["_field"] - b["_field"]) / b["_field"])
  |};

  "duration literal, all units", infer {|
    dur = 1y3mo2w1d4h1m30s1ms2µs70ns
  |};

  "duration literal, months", infer {|
    dur = 6mo
  |};

  "duration literal, milliseconds", infer {|
    dur = 500ms
  |};

  "duration literal, months, minutes, milliseconds", infer {|
    dur = 6mo30m500ms
  |};
]

let () =
  let open Core_bench in
  let open Core in
  List.map benchmarks ~f:(fun (name, benchmark) -> Bench.Test.create ~name benchmark)
  |> Bench.make_command
  |> Command.run