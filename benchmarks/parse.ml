open Ofluxl

let parse str () =
  Syntax.Parse.str str

let benchmarks = [
  "from", parse {|
    from
  |};

  "comment", parse {|
    // Comment
    from()
  |};

  "identifier with number", parse {|
    tan2()
  |};

  "regex literal", parse {|
    /.*/
  |};

  "regex literal with escape sequence", parse {|
    /a\/b\\c\d/
  |};

  "regex match operators", parse {|
    "a" =~ /.*/ and "b" !~ /c$/
  |};

  "declare variable as an int", parse {|
    howdy = 1
  |};

  "declare variable as a float", parse {|
    howdy = 1.1
  |};

  "declare variable as an array", parse {|
    howdy = [1, 2, 3, 4]
  |};

  "use variable to declare something", parse {|
    howdy = 1
    from()
  |};

  "variable is from statement", parse {|
    howdy = from()
    howdy.count()
  |};

  "pipe expression", parse {|
    from() |> count()
  |};

  "literal pipe expression", parse {|
    5 |> pow2()
  |};

  "member expression pipe expression", parse {|
    foo.bar |> baz()
  |};

  "multiple pipe expressions", parse {|
    from() |> range() |> filter() |> count()
  |};

  "two variables for two froms", parse {|
    howdy = from()
    doody = from()
    howdy|>count()
    doody|>sum()
  |};

  "from with database", parse {|
    from(bucket:"telegraf/autogen")
  |};

  "map member expressions", parse {|
    m = {key1: 1, key2:"value2"}
    m.key1
    m["key2"]
  |};

  "index expression", parse {|
    a[3]
  |};

  "nested index expression", parse {|
    a[3][5]
  |};

  "access indexed object returned from function call", parse {|
    f()[3]
  |};

  "index with member expressions", parse {|
    a.b["c"]
  |};

  "index with member with call expression", parse {|
    a.b()["c"]
  |};

  "var as binary expression of other vars", parse {|
    a = 1
    b = 2
    c = a + b
    d = a
  |};

  "var as unary expression of other vars", parse {|
    a = 5
    c = -a
  |};

  "var as both binary and unary expressions", parse {|
    a = 5
    c = 10 * -a
  |};

  "unary expressions within logical expression", parse {|
    a = 5.0
    10.0 * -a == -0.5 or a == 6.0
  |};

  "unary expressions with too many comments", parse {|
    // define a
    a = 5.0
    // eval this
    10.0 * -a == -0.5
    // or this
    or a == 6.0
  |};

  "expressions with function calls", parse {|
    a = foo() == 10
  |};

  "mix unary logical and binary expressions", parse {|
    not (f() == 6.0 * x) or fail()
  |};

  "mix unary logical and binary expressions with extra parens", parse {|
    (not (f() == 6.0 * x) or fail())
  |};

  "arrow function called", parse {|
    plusOne = (r) => r + 1
    plusOne(r:5)
  |};

  "arrow function return map", parse {|
    toMap = (r) =>({r:r})
  |};

  "arrow function with default arg", parse {|
    addN = (r, n=5) => r + n
  |};

  "arrow function called in binary expression", parse {|
    plusOne = (r) => r + 1
    plusOne(r:5) == 6 or die()
  |};

  "arrow function as single expression", parse {|
    f = (r) => r["_measurement"] == "cpu"
  |};

  "arrow function as block", parse {|
    f = (r) => { 
      m = r["_measurement"]
      return m == "cpu"
    }
  |};

  "from with filter with no parens", parse {|
    from(bucket:"telegraf/autogen").filter(fn: (r) => r["other"]=="mem" and r["this"]=="that" or r["these"]!="those")
  |};

  "from with range", parse {|
    from(bucket:"telegraf/autogen")|>range(start:-1h, end:10m)
  |};

  "from with limit", parse {|
    from(bucket:"telegraf/autogen")|>limit(limit:100, offset:10)
  |};

  "from with range and count", parse {|
    from(bucket:"mydb/autogen")
    |> range(start:-4h, stop:-2h)
    |> count()
  |};

  "from with range, limit and count", parse {|
    from(bucket:"mydb/autogen")
    |> range(start:-4h, stop:-2h)
    |> limit(n:10)
    |> count()
  |};

  "from with join", parse {|
    a = from(bucket:"dbA/autogen") |> range(start:-1h)
    b = from(bucket:"dbB/autogen") |> range(start:-1h)
    join(tables:[a,b], on:["host"], fn: (a,b) => a["_field"] + b["_field"])
  |};

  "from with join with complex expression", parse {|
    a = from(bucket:"Flux/autogen")
    |> filter(fn: (r) => r["_measurement"] == "a")
    |> range(start:-1h)

    b = from(bucket:"Flux/autogen")
    |> filter(fn: (r) => r["_measurement"] == "b")
    |> range(start:-1h)

    join(tables:[a,b], on:["t1"], fn: (a,b) => (a["_field"] - b["_field"]) / b["_field"])
  |};

  "duration literal, all units", parse {|
    dur = 1y3mo2w1d4h1m30s1ms2µs70ns
  |};

  "duration literal, months", parse {|
    dur = 6mo
  |};

  "duration literal, milliseconds", parse {|
    dur = 500ms
  |};

  "duration literal, months, minutes, milliseconds", parse {|
    dur = 6mo30m500ms
  |};
]

let () =
  let open Core_bench in
  let open Core in
  List.map benchmarks ~f:(fun (name, benchmark) -> Bench.Test.create ~name benchmark)
  |> Bench.make_command
  |> Command.run