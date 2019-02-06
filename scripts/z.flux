z =
	(f) =>
		((g) => g(g: g))(g: (g) => {
			h = g;
			return f(g: (g) => h(g: h)(g: g))
		});

fib = z(f: (g) => {
	fib = g;
	return (g) => {
		n = g;
		return n < 2 ? n : fib(g: n - 1) + fib(g: n - 2)
	}
});

fib(g: 9);